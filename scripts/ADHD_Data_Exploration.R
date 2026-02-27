# 0. Setup ----
library(data.table)
library(stringr)
library(lubridate)
library(zoo)

zip_path <- "ADHD_Dataset/ADHD-comment.csv.zip"
csv_inside_zip <- "ADHD-comment.csv"

dt <- fread(
  cmd = paste("unzip -p", shQuote(zip_path), shQuote(csv_inside_zip)),
  encoding = "UTF-8",
  showProgress = TRUE
)

# 1. Data Preparation ----

# clean "body"
dt <- dt[!is.na(body)]
dt <- dt[nchar(body) > 0]

# clean placeholder
dt <- dt[!body %in% c("[deleted]", "[removed]")]

# time
dt[, created_datetime := ymd_hms(created_datetime, tz = "UTC")]
dt <- dt[!is.na(created_datetime)]

# clean text
clean_text <- function(x){
  x <- str_to_lower(x)
  x <- str_replace_all(x, "http\\S+|www\\S+", " ")
  x <- str_replace_all(x, "[\\r\\n\\t]+", " ")
  x <- str_squish(x)
  x
}
dt[, body_clean := clean_text(body)]

dt[1:5, .(body, body_clean)]

# aggregate by quarter 2017Q1-2021Q4 (20 quarters)
dt[, qtr := as.yearqtr(created_datetime)]
dt[, qtr_chr := format(qtr, "%YQ%q")]

dt_w <- dt[qtr_chr >= "2016Q3" & qtr_chr <= "2021Q2"]
dt_w[, .N, by = qtr_chr][order(qtr_chr)]

# sample from each quarter
set.seed(570)
n_per_qtr <- 15000

dt_w_s <- dt_w[, .SD[sample(.N, min(.N, n_per_qtr))], by = qtr_chr]

# aggregation
docs_qtr <- dt_w_s[, .(text = paste(body_clean, collapse = " ")), by = qtr_chr]
setorder(docs_qtr, qtr_chr)

docs_qtr
nrow(docs_qtr)
nchar(docs_qtr$text[1])

# 2. TF-IDF (keep stopwords + keep abbreviations; remove ONLY real non-linguistic noise) ----
library(quanteda)
library(quanteda.textstats)
library(dplyr)
library(tidyr)
library(stringr)

# Build corpus from quarter-documents
corp <- corpus(docs_qtr$text, docnames = docs_qtr$qtr_chr)
docvars(corp, "qtr") <- docs_qtr$qtr_chr
summary(corp, n = 3)

# Tokenize (KEEP stopwords + KEEP abbreviations)
toks <- tokens(
  corp,
  remove_punct = TRUE,
  remove_symbols = TRUE,
  remove_numbers = TRUE
)

# Remove ONLY non-linguistic artifacts (HTML/junk/moderation boilerplate)
nonling_noise <- c(
  # HTML / encoding junk
  "amp","nbsp","lt","gt",
  # moderation/system artifacts
  "removal","removed","remove","moderator","moderators","mod","mods",
  "automoderator","bot","bots","rule","rules","ban","banned","deleted",
  # artifacts that sometimes appear in reddit exports
  "rectangle","totesmessenger","faq","faqs","opt-out","optout",
  "click","link","http","https","www"
)

toks_clean <- tokens_remove(toks, pattern = nonling_noise)

# DFM + trim (stability + reduce idiosyncratic one-off terms)
dfm_counts <- dfm(toks_clean)

# remove very rare terms (spelling errors / one-off tokens)
dfm_counts <- dfm_trim(dfm_counts, min_termfreq = 30)

# remove extremely common terms (almost everywhere; little discriminative power)
dfm_counts <- dfm_trim(dfm_counts, max_docfreq = 0.98, docfreq_type = "prop")

dfm_counts

# TF-IDF
dfm_tfidf <- dfm_tfidf(dfm_counts)

# Extract top TF-IDF terms per document
top_n <- 15

top_terms <- lapply(docnames(dfm_tfidf), function(dn){
  v <- as.numeric(dfm_tfidf[dn, ])
  names(v) <- featnames(dfm_tfidf)
  ord <- order(v, decreasing = TRUE)
  
  data.frame(
    doc = dn,
    term = names(v)[ord][1:top_n],
    tfidf = v[ord][1:top_n],
    row.names = NULL
  )
}) |>
  bind_rows() |>
  arrange(doc, desc(tfidf))

top_terms_wide <- top_terms |>
  group_by(doc) |>
  summarise(top_terms = paste(term, collapse = ", "), .groups = "drop")

head(top_terms_wide, 5)
top_terms |> filter(doc == docs_qtr$qtr_chr[1])

# 3. Pearson Similarity ----
library(quanteda.textstats)

sim_r <- textstat_simil(dfm_counts, margin = "documents", method = "correlation")
r_mat <- as.matrix(sim_r)

dim(r_mat)
round(r_mat[1:5, 1:5], 3)

# heatmap
library(ggplot2)
library(tibble)

heat_df <- as.data.frame(r_mat) |>
  rownames_to_column("doc_i") |>
  pivot_longer(-doc_i, names_to = "doc_j", values_to = "r")

ggplot(heat_df, aes(x = doc_j, y = doc_i, fill = r)) +
  geom_tile() +
  coord_fixed() +
  scale_fill_gradient2(midpoint = 0) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Pearson Correlation Between Quarter Documents",
    x = NULL, y = NULL, fill = "r"
  )

# find the most similar pairs and most dissimilar pairs
pairs <- heat_df |>
  filter(doc_i != doc_j) |>
  mutate(pair = ifelse(doc_i < doc_j,
                       paste(doc_i, doc_j, sep = " ~ "),
                       paste(doc_j, doc_i, sep = " ~ "))) |>
  group_by(pair) |>
  summarise(r = mean(r), .groups = "drop")

most_similar <- pairs |> arrange(desc(r)) |> slice_head(n = 2)
least_similar <- pairs |> arrange(r) |> slice_head(n = 2)

most_similar
least_similar

# 4. Syntax Complexity
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(udpipe)

# Pick the least similar quarter pair
qA <- "2017Q1"
qB <- "2020Q4"

# =Filter to "mostly English" comments (reduce German noise)
is_mostly_english <- function(x, min_ascii_ratio = 0.95, min_letter_ratio = 0.50){
  ascii_ratio  <- str_count(x, "[\\x00-\\x7F]") / pmax(nchar(x), 1)
  letter_ratio <- str_count(x, "[A-Za-z]") / pmax(nchar(x), 1)
  ascii_ratio >= min_ascii_ratio & letter_ratio >= min_letter_ratio
}

# Sample comments, split to sentences, sample sentences for parsing
set.seed(570)
n_comments <- 3000
n_sentences_parse <- 1000

A_comments <- dt_w_s[qtr_chr == qA, .(text = body_clean)]
B_comments <- dt_w_s[qtr_chr == qB, .(text = body_clean)]

A_comments <- A_comments[!is.na(text) & nchar(text) > 0]
B_comments <- B_comments[!is.na(text) & nchar(text) > 0]

A_comments <- A_comments[is_mostly_english(text)]
B_comments <- B_comments[is_mostly_english(text)]

A_comments <- A_comments[sample(.N, min(.N, n_comments))]
B_comments <- B_comments[sample(.N, min(.N, n_comments))]

make_sentence_df <- function(text_vec, label){
  all_text <- paste(text_vec, collapse = " ")
  sents <- unlist(strsplit(all_text, "(?<=[.!?])\\s+", perl = TRUE))
  sents <- str_squish(sents)
  sents <- sents[!is.na(sents) & nchar(sents) > 0]
  data.frame(group = label, sentence = sents, stringsAsFactors = FALSE)
}

sent_A <- make_sentence_df(A_comments$text, qA)
sent_B <- make_sentence_df(B_comments$text, qB)

# filter extremely long sentences
sent_A <- sent_A |> filter(nchar(sentence) <= 400)
sent_B <- sent_B |> filter(nchar(sentence) <= 400)

nA <- min(nrow(sent_A), n_sentences_parse)
nB <- min(nrow(sent_B), n_sentences_parse)

set.seed(570)
sent_A <- sent_A |> slice_sample(n = nA)
sent_B <- sent_B |> slice_sample(n = nB)

sent_all <- bind_rows(sent_A, sent_B) |>
  mutate(sent_id = row_number(),
         doc_id = paste0(group, "_", sent_id))

cat("Sentences to parse:", nrow(sent_all), "\n")

# Load UDPipe English model
model_file <- udpipe_download_model(language = "english-ewt")$file_model
ud_model <- udpipe_load_model(model_file)

# Parse sentences
anno <- udpipe_annotate(ud_model, x = sent_all$sentence, doc_id = sent_all$doc_id)
anno <- as.data.frame(anno)

# Clean columns + standardize names
anno2 <- anno |>
  transmute(
    doc_id,
    sentence_id,
    token_id,
    token,
    lemma,
    upos,
    head_token_id,
    deprel = dep_rel
  ) |>
  left_join(sent_all |> select(doc_id, group, sentence), by = "doc_id")

# sanity check
stopifnot(all(c("doc_id","group","sentence","sentence_id","token_id","upos","head_token_id","deprel") %in% names(anno2)))

# Dependent clause relations (UD)
dep_clause_rels <- c("ccomp","xcomp","advcl","acl","acl:relcl")

# Coordination (UD): coordinated units are typically marked by 'conj'
coord_rels <- c("conj")

# Complex nominals: noun heads with nominal modifiers (proxy consistent with UD patterns)
cn_mod_rels <- c("amod","nmod","compound","appos","nummod","acl","acl:relcl")

# Clause head proxy: verbal heads of clause units
is_clause_head <- function(upos, deprel){
  upos %in% c("VERB","AUX") && (deprel %in% c("root", dep_clause_rels, "conj"))
}

# Compute per-sentence counts + required ratios
sent_metrics <- anno2 |>
  group_by(doc_id, group, sentence_id, sentence) |>
  summarise(
    # Sentence length tokens (MLS uses tokens per sentence)
    tokens = sum(!is.na(token_id)),
    
    # Clause count (C): count clause heads
    clauses = sum(mapply(is_clause_head, upos, deprel)),
    
    # Dependent clauses (DC): count of dependent-clause relations
    dep_clauses = sum(deprel %in% dep_clause_rels),
    
    # Coordination: count of conj relations
    coordinations = sum(deprel %in% coord_rels),
    
    # Complex nominals: count noun heads with at least one nominal-modifier relation
    complex_nominals = {
      noun_heads <- which(upos %in% c("NOUN","PROPN","PRON"))
      if (length(noun_heads) == 0) 0 else {
        heads <- token_id[noun_heads]
        sum(vapply(heads, function(h){
          any(head_token_id == h & deprel %in% cn_mod_rels)
        }, logical(1)))
      }
    },
    .groups = "drop"
  ) |>
  mutate(
    # avoid divide-by-zero
    clauses = pmax(clauses, 1),
    
    # Required measures
    C_S = clauses,                  # Clauses per Sentence
    DC_S = dep_clauses,             # Dependent Clauses per Sentence
    DC_C = dep_clauses / clauses,   # Dependent Clauses per Clause
    Coord_S = coordinations,        # Coordination per Sentence
    Coord_C = coordinations / clauses, # Coordination per Clause
    CN_S = complex_nominals,        # Complex Nominals per Sentence
    CN_C = complex_nominals / clauses  # Complex Nominals per Clause
  )

# Summary table
syntax_summary <- sent_metrics |>
  group_by(group) |>
  summarise(
    n_sentences = n(),
    MLS = mean(tokens),
    C_S = mean(C_S),
    DC_S = mean(DC_S),
    DC_C = mean(DC_C),
    Coord_S = mean(Coord_S),
    Coord_C = mean(Coord_C),
    CN_S = mean(CN_S),
    CN_C = mean(CN_C),
    .groups = "drop"
  ) |>
  arrange(group)

syntax_summary

# Example sentences
pick_examples <- function(df, grp){
  df_g <- df |> filter(group == grp)
  ex_dep <- df_g |> arrange(desc(DC_C), desc(C_S), desc(tokens)) |> slice(1)
  ex_cn  <- df_g |> arrange(desc(CN_C), desc(C_S), desc(tokens)) |> slice(1)
  bind_rows(
    ex_dep |> mutate(example_type = "High dependent-clause density"),
    ex_cn  |> mutate(example_type = "High complex-nominal density")
  ) |>
    select(group, example_type, sentence, tokens, clauses, dep_clauses, coordinations, complex_nominals, DC_C, CN_C)
}

examples <- bind_rows(
  pick_examples(sent_metrics, qA),
  pick_examples(sent_metrics, qB)
)

examples

# difference table (B - A)
syntax_diff <- syntax_summary |>
  pivot_longer(-group, names_to = "metric", values_to = "value") |>
  pivot_wider(names_from = group, values_from = value) |>
  mutate(diff_B_minus_A = .data[[qB]] - .data[[qA]])

syntax_diff

# additional result
top_terms_wide