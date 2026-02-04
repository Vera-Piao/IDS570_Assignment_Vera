# 1. Setup ----
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(tidytext)

path_circle <- "texts/A07594__Circle_of_Commerce.txt"
path_free   <- "texts/B14801__Free_Trade.txt"

circle_raw <- read_file(path_circle)
free_raw   <- read_file(path_free)

texts_tbl <- tibble(
  doc_title = c("Circle of Commerce", "Free Trade"),
  text      = c(circle_raw, free_raw)
)

# 2. Raw-Count Sentiment Analysis ----
# Step 1: tokenize and clean the texts
# a. Basic Normalization (long s into s)
texts_tbl <- texts_tbl %>%
  mutate(
    text_norm = text %>%
      str_replace_all("ſ", "s") %>%
      str_to_lower()
  )

# b. tokenize and remove the stopwords
data("stop_words")

tidy_words <- texts_tbl %>%
  select(doc_title, text_norm) %>%
  unnest_tokens(word, text_norm, token = "words") %>%
  anti_join(stop_words, by = "word")

# Step 2: Use Bing to label words into positive or negative (sentiment analysis)
bing <- get_sentiments("bing")

sentiment_words_raw <- tidy_words %>%
  inner_join(bing, by = "word")

# Step 3: Compute raw sentiment totals
raw_sentiment_summary <- sentiment_words_raw %>%
  group_by(doc_title) %>%
  summarise(
    raw_positive = sum(sentiment == "positive"),
    raw_negative = sum(sentiment == "negative"),
    net_sentiment_raw = raw_positive - raw_negative,
    .groups = "drop"
  )

# 3. TF-IDF–Weighted Sentiment Analysis ----
# Step 4: Compute TF-IDF for words in each document
word_counts <- tidy_words %>%
  count(doc_title, word, name = "n")

tfidf_tbl <- word_counts %>%
  bind_tf_idf(term = word, document = doc_title, n = n)

# Step 5: Keep only sentiment-bearing words
sentiment_words_tfidf <- tfidf_tbl %>%
  inner_join(bing, by = "word")

# Step 6: Compute TF-IDF–weighted sentiment totals
tfidf_sentiment_summary <- sentiment_words_tfidf %>%
  group_by(doc_title) %>%
  summarise(
    tfidf_positive = sum(tf_idf[sentiment == "positive"], na.rm = TRUE),
    tfidf_negative = sum(tf_idf[sentiment == "negative"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    tfidf_positive = replace_na(tfidf_positive, 0),
    tfidf_negative = replace_na(tfidf_negative, 0),
    net_sentiment_tfidf = tfidf_positive - tfidf_negative
  )

# 4. Compare Raw vs. TF-IDF Sentiment ----
final_sentiment_comparison <- raw_sentiment_summary %>%
  left_join(tfidf_sentiment_summary, by = "doc_title")

write_csv(final_sentiment_comparison, "output/sentiment_comparison.csv")

# 5. Challenge Question ----
# Question 1: total TF-IDF sentiment per document
total_sentiment_tfidf <- sentiment_words_tfidf %>%
  group_by(doc_title) %>%
  summarise(
    total_sentiment_weight = sum(tf_idf, na.rm = TRUE),
    .groups = "drop"
  )

# Question 2: top 5 sentiment words per document
top5_sentiment_words <- sentiment_words_tfidf %>%
  group_by(doc_title) %>%
  arrange(desc(tf_idf), .by_group = TRUE) %>%
  slice_head(n = 5) %>%
  ungroup()

# Question 3: proportion of sentiment of top 5 words
top5_share <- top5_sentiment_words %>%
  group_by(doc_title) %>%
  summarise(
    top5_sentiment_weight = sum(tf_idf, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(total_sentiment_tfidf, by = "doc_title") %>%
  mutate(
    top5_proportion = ifelse(total_sentiment_weight == 0, NA_real_,
                             top5_sentiment_weight / total_sentiment_weight)
  )