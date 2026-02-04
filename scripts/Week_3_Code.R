# 1. Fixing Spelling with Regex ----

library(readr) 
library(dplyr) 
library(stringr) 
library(tibble)  

circle_raw <- read_file("texts/A07594__Circle_of_Commerce.txt") 

text_tbl <- tibble(   
  doc_title = "The Circle of Commerce",   
  text = circle_raw 
)  

text_tbl %>% select(doc_title)

nchar(text_tbl$text)

str_sub(text_tbl$text, 1, 400)

# backup copy
text_tbl <- text_tbl %>%
  mutate(text_original = text)

# Regular Expression Regex

text_tbl <- text_tbl %>%   
  mutate(     
    text_clean = str_replace_all(text_original, "ſ", "s")   
  )  

# Let's count how many "ſ" we had before and after to check that the substitution worked:
tibble(
  long_s_before = str_count(text_tbl$text_original, "ſ"),
  long_s_after  = str_count(text_tbl$text_clean, "ſ")
) # the output shows that no more "ſ" left.

# deal with punctuation
text_tbl <- text_tbl %>%
  mutate(
    text_clean = str_replace_all(text_clean, regex("[[:punct:]&&[^£'-]]"), " ") 
    # writing over "text_clean", we could also create a new column instead
  )

# Standardize name
name_map <- c(   
  "Smythe" = "Smith",   "Smyth"  = "Smith",   "Smithe" = "Smith" 
)

text_standard <- text_tbl %>%
  mutate(
    text_norm = text_clean %>%
      str_replace_all(regex("\\bSmythe\\b", ignore_case = TRUE), "Smith") %>%
      str_replace_all(regex("\\bSmyth\\b",  ignore_case = TRUE), "Smith") %>%
      str_replace_all(regex("\\bSmithe\\b", ignore_case = TRUE), "Smith")
  )
# \\b to denote word boundary

# 2. Describing the Text: N-grams and Trade ----
library(tidyr) 
library(tidytext) 
library(ggplot2) 
library(forcats)

# tokenizing into bigrams
bigrams_raw <- text_standard %>%
  select(doc_title, text_norm) %>%
  unnest_tokens(output = "bigram", input = text_norm, token = "ngrams", n = 2)

bigrams_raw %>% count(bigram, sort = TRUE) %>% slice_head(n = 10)

# remove stop words
data("stop_words") # the standard list from tidytext, but you can adapt the process from week 2 to include custom stop words

bigrams_clean <- bigrams_raw %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(str_detect(word1, "^[a-z]+$")) %>% # note: here I am removing ALL punctuation (earlier we kept specific symbols)
  filter(str_detect(word2, "^[a-z]+$"))

bigrams_clean %>% count(word1, word2, sort = TRUE) %>% slice_head(n = 10)

# glue the bigram back after cleaning the stop words
bigram_counts <- bigrams_clean %>%
  count(word1, word2, sort = TRUE) %>%
  unite("bigram", word1, word2, sep = " ")

bigram_counts %>%
  slice_head(n = 20) %>%
  mutate(bigram = fct_reorder(bigram, n)) %>%
  ggplot(aes(x = n, y = bigram)) +
  geom_col() +
  labs(
    title = "Most frequent bigrams (after stopword filtering)",
    x = "Count",
    y = NULL
  )

# 2 strategies: 
# a. Filter bigrams that literally contain the token trade
# b. Use a small “trade lexicon” to capture near-synonyms (e.g., traffick, commerce, merchant, exchange)

# a. which bigrams contain the token trade
trade_bigrams <- bigram_counts %>%
  filter(str_detect(bigram, "\\btrade\\b"))

trade_bigrams %>% slice_head(n = 25)

trade_bigrams %>%
  slice_head(n = 20) %>%
  mutate(bigram = fct_reorder(bigram, n)) %>%
  ggplot(aes(x = n, y = bigram)) +
  geom_col() +
  labs(
    title = "Bigrams that include the word 'trade'",
    x = "Count",
    y = NULL
  )

# b. first we create a lexicon
trade_lexicon <- c(
  "trade", "traffick", "traffic", "commerce", "merchant", "merchants",
  "exchange", "export", "import", "commodity", "commodities",
  "navigation", "shipping", "market", "markets"
)

trade_theme_bigrams <- bigrams_clean %>%
  filter(word1 %in% trade_lexicon | word2 %in% trade_lexicon) %>%
  count(word1, word2, sort = TRUE) %>%
  unite("bigram", word1, word2, sep = " ")

trade_theme_bigrams %>% slice_head(n = 25)

trade_theme_bigrams %>%
  slice_head(n = 20) %>%
  mutate(bigram = fct_reorder(bigram, n)) %>%
  ggplot(aes(x = n, y = bigram)) +
  geom_col() +
  labs(
    title = "Trade-theme bigrams (lexicon-based)",
    x = "Count",
    y = NULL
  )

# 3. Sentiment Analysis ----
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)

circle_raw <- read_file("texts/A07594__Circle_of_Commerce.txt")
free_raw   <- read_file("texts/B14801__Free_Trade.txt")

texts_miss <- tibble(
  doc_title = c("Circle of Commerce", "Free Trade"),
  text = c(circle_raw, free_raw)
)

texts_miss <- texts_miss %>%
  mutate(
    text_norm = text %>%
      str_replace_all("ſ", "s") %>%   # long s as above
      str_replace_all("\\s+", " ") %>% # collapse whitespace
      str_to_lower()
  )

# tracking the position of each word
tokens <- texts_miss %>%
  unnest_tokens(word, text_norm, token = "words") %>%
  group_by(doc_title) %>%
  mutate(token_id = row_number()) %>%
  ungroup()

# locate our word position
trade_terms <- c("trade", "commerce", "merchant", "merchants")

trade_hits <- tokens %>%
  filter(word %in% trade_terms) %>%
  select(doc_title, hit_word = word, hit_token_id = token_id)

# create token window
window_size <- 30

trade_windows <- tokens %>%
  inner_join(trade_hits, by = "doc_title") %>%
  filter(token_id >= hit_token_id - window_size,
         token_id <= hit_token_id + window_size) %>%
  mutate(window_id = paste(doc_title, hit_token_id, sep = "_"))

# look at one of the windows
trade_windows %>%
  filter(window_id == nth(unique(window_id), 10)) %>%
  summarise(window_text = str_c(word, collapse = " ")) %>%
  pull(window_text) %>%
  cat()

# start sentiment analysis
bing <- get_sentiments("bing")

window_sentiment <- trade_windows %>%
  inner_join(bing, by = "word") %>%  # keeps only sentiment-bearing words
  count(doc_title, window_id, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(net_sentiment = positive - negative)

# create an overall summary of sentiments for each text
text_sentiment_summary <- window_sentiment %>%
  group_by(doc_title) %>%
  summarise(
    windows = n(),
    total_positive = sum(positive),
    total_negative = sum(negative),
    total_net_sentiment = sum(net_sentiment),      
    avg_net_per_window = mean(net_sentiment),
    .groups = "drop"
  )
text_sentiment_summary

# plot the distribution
ggplot(window_sentiment, aes(x = net_sentiment)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ doc_title, ncol = 1) +
  labs(
    title = "Sentiment in Trade-Centered Windows (±30 words)",
    x = "Net sentiment (positive - negative) per window",
    y = "Number of trade windows"
  )
