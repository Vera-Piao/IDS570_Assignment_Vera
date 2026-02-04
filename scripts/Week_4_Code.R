# 1. Document-Feature Matrices ----
library(tibble)
library(dplyr)
library(readr)
library(ggplot2)
library(quanteda)
library(quanteda.textstats)

txt_circle    <- read_file("texts/A07594__Circle_of_Commerce.txt")
txt_free      <- read_file("texts/B14801__Free_Trade.txt")
txt_third     <- read_file("texts/A06785.txt")

texts <- c(
  "Circle_of_Commerce" = txt_circle,
  "Free_Trade"         = txt_free,
  "Third_Text_A06785"  = txt_third
)

corp <- corpus(texts)

# tokenization and cleaning
toks <- tokens(
  corp,
  remove_punct   = TRUE,
  remove_numbers = TRUE,
  remove_symbols = TRUE
)

toks <- tokens_tolower(toks)

custom_stop <- c(
  "vnto","haue","doo","hath","bee","ye","thee","hee","shall","hast","doe",
  "beene","thereof","thus" 
)

toks <- tokens_remove(toks, pattern = c(stopwords("en"), custom_stop))

# build the DFM
# Document-feature matrix (DFM)
dfm_mat <- dfm(toks)

# Inspect by raw count (in our corpus) the top 25 features 
dfm_mat

topfeatures(dfm_mat, 25)

# 2. Correlation measure ----
# Pearson correlation similarity
sim_cor <- textstat_simil(
  dfm_mat,
  method = "correlation",
  margin = "documents"
)
sim_cor

# Cosine similarity
sim_cos <- textstat_simil(
  dfm_mat,
  method = "cosine",
  margin = "documents"
)

sim_cos

# 3. TF-IDF ----
dfm_tfidf <- dfm_tfidf(dfm_mat)

dfm_tfidf

topfeatures(dfm_tfidf, 20)
tfidf_mat <- as.matrix(dfm_tfidf)

# Circle of Commerce
circle_tfidf <- tfidf_mat["Circle_of_Commerce", ]

# Sort and get top 20
top_circle <- sort(circle_tfidf, decreasing = TRUE)[1:20]
top_circle

# Free Trade
free_tfidf <- tfidf_mat["Free_Trade", ]

# Sort and get top 20
top_free <- sort(free_tfidf, decreasing = TRUE)[1:20]
top_free

# A06786
A06785_tfidf <- tfidf_mat["Third_Text_A06785", ]

# Sort and get top 20
top_A06785 <- sort(A06785_tfidf, decreasing = TRUE)[1:20]
top_A06785

# 4. Visualization ----
tfidf_top_tbl <- bind_rows(
  tibble(document = "Circle of Commerce", term = names(top_circle), tfidf = unname(top_circle)),
  tibble(document = "Free Trade",         term = names(top_free),   tfidf = unname(top_free)),
  tibble(document = "Third Text",         term = names(top_A06785),  tfidf = unname(top_A06785))
)

ggplot(tfidf_top_tbl, aes(x = tfidf, y = reorder(term, tfidf))) +
  geom_col() +
  facet_wrap(~ document, scales = "free_y") +
  labs(
    title = "Most Characteristic Terms by Document (TF–IDF)",
    x = "TF–IDF score",
    y = NULL
  ) +
  theme_minimal()
