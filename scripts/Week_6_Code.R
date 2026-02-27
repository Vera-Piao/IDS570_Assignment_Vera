library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(tibble)
library(readr) 
library(tidyr) 
library(tidyverse)
library(ggplot2)

raw_text <- read_file("texts/wealth.txt")

texts <- tibble(
  doc_title = "wealth.txt",
  text = raw_text
)

texts

tokens <- texts %>%
  unnest_tokens(word, text) %>%
  group_by(doc_title) %>%
  mutate(token_id = row_number()) %>%
  ungroup()

#Let's take a quick look 
tokens %>% slice(1:20)

anchor <- "labor" #this allows you to change this as needed
window_size <- 5

#check how many anchor words are in the text
tokens %>% filter(word == anchor) %>% count()

anchor_hits <- tokens %>%
  filter(word == anchor) %>%
  select(doc_title, anchor_id = token_id) # organize by location of anchor position

#Let's take a look at the tibble
anchor_hits

# Each row in `windows` will be ONE token that appears near ONE anchor occurrence.

windows <- anchor_hits %>%
  # Join each anchor occurrence to ALL tokens in the same document.
  # This is a "many-to-many" join because one anchor hit matches many tokens [remember we had a similar set up last week].
  left_join(tokens, by = "doc_title", relationship = "many-to-many") %>%
  
  # Compute how far each token is from the anchor.
  # Negative distance = to the LEFT of the anchor; positive = to the RIGHT.
  mutate(distance = token_id - anchor_id) %>%
  
  # Keep only tokens within the window size (±5 tokens).
  filter(abs(distance) <= window_size) %>%
  
  # Remove the anchor word itself (distance 0) so we only keep context words, which is what we are interested in.
  filter(distance != 0) %>%
  
  
  mutate(
    window_id = paste0(doc_title, "_", anchor_id),
    anchor_word = anchor
  ) %>%
  
  # Keep only the columns we need for the next steps.
  select(
    doc_title,
    window_id,
    anchor_word,
    anchor_id,
    token_id,
    distance,
    word
  )

windows %>%
  arrange(anchor_id, distance) %>%
  slice(1:20)

cooc <- windows %>%
  count(word, sort = TRUE, name = "cooc_n")

cooc

total_tokens <- nrow(tokens)

word_freq <- tokens %>%
  count(word, name = "word_n") %>%
  mutate(p_word = word_n / total_tokens)

word_freq

anchor_stats <- word_freq %>%
  filter(word == anchor) %>%
  transmute(anchor_n = word_n, p_anchor = p_word)

anchor_stats

total_window_tokens <- nrow(windows)

p_w_given_windows <- windows %>%
  count(word, name = "cooc_n") %>%
  mutate(p_word_in_windows = cooc_n / total_window_tokens)

p_w_given_windows

pmi_tbl <- p_w_given_windows %>%
  left_join(word_freq, by = "word") %>%
  mutate(
    pmi = log2(p_word_in_windows / (p_word * anchor_stats$p_anchor))
  ) %>%
  arrange(desc(pmi))

pmi_tbl

pmi_tbl_filtered <- pmi_tbl %>%
  filter(cooc_n >= 3) %>%        # adjust threshold as needed
  arrange(desc(pmi))

pmi_tbl_filtered

cooc %>%
  slice_max(cooc_n, n = 15) %>%
  mutate(word = reorder(word, cooc_n)) %>%
  ggplot(aes(x = cooc_n, y = word)) +
  geom_col() +
  labs(
    title = str_glue("Top co-occurring words within ±{window_size} of '{anchor}'"),
    x = "Co-occurrence count",
    y = NULL
  )

pmi_tbl_filtered %>%
  slice_max(pmi, n = 15) %>%
  mutate(word = reorder(word, pmi)) %>%
  ggplot(aes(x = pmi, y = word)) +
  geom_col() +
  labs(
    title = str_glue("Top PMI-associated words near '{anchor}' (cooc_n ≥ 3)"),
    x = "PMI (log2 scale)",
    y = NULL
  )
