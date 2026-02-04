# setup ----
#import packages
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(forcats)
library(tibble)
library(scales)

# read documents
file_a <- "../texts/A07594__Circle_of_Commerce.txt"
file_b <- "../texts/B14801__Free_Trade.txt"

text_a <- read_file(file_a)
text_b <- read_file(file_b)

texts <- tibble(
  doc_title = c("Text A", "Text B"),
  text = c(text_a, text_b)
)

# Diagnostics Table ----
# n_chars: number of characters in each doc
char_counts <- texts %>%
  mutate(n_chars = str_length(text)) %>%
  select(doc_title, n_chars)

# tokenized words
tokens_raw <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word))

# n_word_tokens: total tokens
token_counts <- tokens_raw %>%
  group_by(doc_title) %>%
  summarise(n_word_tokens = n(), .groups = "drop")

# n_word_types: unique word types
type_counts <- tokens_raw %>%
  group_by(doc_title) %>%
  summarise(n_word_types = n_distinct(word), .groups = "drop")

corpus_diagnostics <- char_counts %>%
  left_join(token_counts, by = "doc_title") %>%
  left_join(type_counts, by = "doc_title")

corpus_diagnostics

# Removal of Stop Words ----
data("stop_words")

# Add project-specific stopwords
custom_stopwords <- tibble(
  word = c(
    "vnto", "haue", "doo", "hath", "bee", "ye", "thee"
  )
)

all_stopwords <- bind_rows(stop_words, custom_stopwords) %>%
  distinct(word)

all_stopwords %>% slice(1:10)

word_counts <- texts %>%
  unnest_tokens(word, text) %>%
  mutate(word = str_to_lower(word)) %>%
  anti_join(all_stopwords, by = "word") %>%
  count(doc_title, word, sort = TRUE)

word_counts

# calculate the total number of words after stopword removal in each document
doc_lengths <- word_counts %>%
  group_by(doc_title) %>%
  summarise(total_words = sum(n))

doc_lengths

# calculate each word's frequency as a proportion of the total words
word_counts_normalized <- word_counts %>%
  left_join(doc_lengths, by = "doc_title") %>%
  mutate(relative_freq = n / total_words)

word_counts_normalized

# filter for "trade" in both documents
trade_compare <- word_counts_normalized %>%
  filter(word == "trade") %>%
  select(doc_title, n, total_words, relative_freq)

trade_compare

# Plotting ----
plot_n_words <- 20

word_comparison_tbl <- word_counts %>%
  pivot_wider(
    names_from = doc_title,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(max_n = pmax(`Text A`, `Text B`)) %>%
  arrange(desc(max_n))

top_words <- word_comparison_tbl %>%
  slice_head(n = plot_n_words) %>%
  pull(word)

word_plot_data <- word_counts_normalized %>%
  filter(word %in% top_words) %>%
  select(word, doc_title, relative_freq) %>%
  mutate(word = fct_reorder(word, relative_freq, .fun = max))

ggplot(word_plot_data, aes(x = relative_freq, y = word)) +
  geom_col() +
  facet_wrap(~ doc_title, scales = "free_x") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(
    title = "Most frequent words",
    subtitle = paste0(
      "Top ", plot_n_words,
      " words shown as relative frequency"
    ),
    x = "Relative frequency (n / total_words)",
    y = NULL
  ) +
  theme_minimal()
