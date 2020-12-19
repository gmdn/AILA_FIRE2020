library(tidytext)
library(textstem)

# On September 23rd, the organizers sent an email with a new set of casedocs.

casedocs_word <- casedocs_split %>% 
  select(doc_id, text) %>%
  unnest_tokens(output = word, input = text) %>%
  mutate(word = ifelse(str_ends(word, "'s"), str_sub(word, end = -3), word)) %>% 
  count(doc_id, word)

saveRDS(casedocs_word, "imported_2020/casedocs_word.rds")

# filter stopwords and the remaining single character words
casedocs_word_filter <- casedocs_word %>%
  anti_join(get_stopwords()) %>%
  filter(nchar(word) > 2 & str_detect(word, "^[a-z]+$"))

saveRDS(casedocs_word_filter, "imported_2020/casedocs_word_filter.rds")

# casedocs_words_filter %>%
#   filter(str_detect(word, "\\."))

casedocs_stem_filter <- casedocs_word_filter %>%
  mutate(stem = stem_words(word, language = "porter")) %>%
  filter(nchar(stem) > 2) %>%
  group_by(doc_id, stem) %>%
  summarize(n = sum(n)) %>%
  ungroup()

saveRDS(casedocs_stem_filter, "imported_2020/casedocs_stem_filter.rds")

casedocs_stem_filter %>%
  arrange(desc(n))

casedocs_stem_filter %>%
  arrange(stem)

casedocs_lemma_filter <- casedocs_word_filter %>%
  mutate(lemma = lemmatize_words(word)) %>%
  filter(nchar(lemma) > 2) %>%
  group_by(doc_id, lemma) %>%
  summarize(n = sum(n)) %>%
  ungroup()

saveRDS(casedocs_lemma_filter, "imported_2020/casedocs_lemma_filter.rds")

casedocs_lemma_filter %>%
  arrange(desc(n)) 

casedocs_lemma_filter %>%
  arrange(lemma) 


