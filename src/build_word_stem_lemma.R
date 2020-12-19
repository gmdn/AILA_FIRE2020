library(tidytext)
library(textstem)

casedocs_word <- casedocs_split %>% 
  select(doc_id, text) %>%
  unnest_tokens(output = word, input = text) %>%
  mutate(word = ifelse(str_ends(word, "'s"), str_sub(word, end = -3), word)) %>% 
  count(doc_id, word)

saveRDS(casedocs_word, "imported/casedocs_word.rds")

# filter stopwords and the remaining single character words
casedocs_word_filter <- casedocs_word %>%
  anti_join(get_stopwords()) %>%
  filter(nchar(word) > 2 & str_detect(word, "^[a-z]+$"))

saveRDS(casedocs_word_filter, "imported/casedocs_word_filter.rds")

# casedocs_words_filter %>%
#   filter(str_detect(word, "\\."))

casedocs_stem_filter <- casedocs_word_filter %>%
  mutate(stem = stem_words(word, language = "porter")) %>%
  filter(nchar(stem) > 2) %>%
  group_by(doc_id, stem) %>%
  summarize(n = sum(n)) %>%
  ungroup()

saveRDS(casedocs_stem_filter, "imported/casedocs_stem_filter.rds")

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

saveRDS(casedocs_lemma_filter, "imported/casedocs_lemma_filter.rds")

casedocs_lemma_filter %>%
  arrange(desc(n)) 

casedocs_lemma_filter %>%
  arrange(lemma) 


##############


statutes_word <- statutes_split %>% 
  rename(text = full_text) %>%
  select(doc_id, text) %>%
  unnest_tokens(output = word, input = text) %>%
  mutate(word = ifelse(str_ends(word, "'s"), str_sub(word, end = -3), word)) %>% 
  count(doc_id, word)

saveRDS(statutes_word, "imported/statutes_word.rds")

# filter stopwords and the remaining single character words
statutes_word_filter <- statutes_word %>%
  anti_join(get_stopwords()) %>%
  filter(nchar(word) > 2 & str_detect(word, "^[a-z]+$"))

saveRDS(statutes_word_filter, "imported/statutes_word_filter.rds")

# statutes_words_filter %>%
#   filter(str_detect(word, "\\."))

statutes_stem_filter <- statutes_word_filter %>%
  mutate(stem = stem_words(word, language = "porter")) %>%
  filter(nchar(stem) > 2) %>%
  group_by(doc_id, stem) %>%
  summarize(n = sum(n)) %>%
  ungroup()

saveRDS(statutes_stem_filter, "imported/statutes_stem_filter.rds")

statutes_stem_filter %>%
  arrange(desc(n))

statutes_stem_filter %>%
  arrange(stem)


statutes_lemma_filter <- statutes_word_filter %>%
  mutate(lemma = lemmatize_words(word)) %>%
  filter(nchar(lemma) > 2) %>%
  group_by(doc_id, lemma) %>%
  summarize(n = sum(n)) %>%
  ungroup()

saveRDS(statutes_lemma_filter, "imported/statutes_lemma_filter.rds")

statutes_lemma_filter %>%
  arrange(desc(n)) 

statutes_lemma_filter %>%
  arrange(lemma) 



#############3







queries_train_word <- queries_train %>%
  unnest_tokens(word, text) %>%
  mutate(word = ifelse(str_ends(word, "'s"), str_sub(word, end = -3), word)) %>%
  count(query, word)

# queries_train_word %>%
#   filter(str_detect(word, "'"))

queries_train_word_filter <- queries_train_word %>%
  anti_join(get_stopwords()) %>%
  filter(nchar(word) > 2 & str_detect(word, "^[a-z]+$"))

saveRDS(queries_train_word_filter, "imported/queries_train_word_filter.rds")

queries_train_stem_filter <- queries_train_word_filter %>%
  mutate(stem = stem_words(word, language = "porter")) %>%
  filter(nchar(stem) > 2) %>%
  group_by(query, stem) %>%
  summarize(n = sum(n)) %>%
  ungroup()

saveRDS(queries_train_stem_filter, "imported/queries_train_stem_filter.rds")

# queries_train_stem_filter %>%
#   arrange(desc(n))

queries_train_lemma_filter <- queries_train_word_filter %>%
  mutate(lemma = lemmatize_words(word)) %>%
  filter(nchar(lemma) > 2) %>%
  group_by(query, lemma) %>%
  summarize(n = sum(n)) %>%
  ungroup()

saveRDS(queries_train_lemma_filter, "imported/queries_train_lemma_filter.rds")

queries_train_lemma_filter %>%
  arrange(desc(n)) 



queries_test_word <- queries_test %>%
  unnest_tokens(word, text) %>%
  mutate(word = ifelse(str_ends(word, "'s"), str_sub(word, end = -3), word)) %>%
  count(query, word)

# queries_test_word %>%
#   filter(str_detect(word, "'"))

queries_test_word_filter <- queries_test_word %>%
  anti_join(get_stopwords()) %>%
  filter(nchar(word) > 2 & str_detect(word, "^[a-z]+$"))

saveRDS(queries_test_word_filter, "imported/queries_test_word_filter.rds")

queries_test_stem_filter <- queries_test_word_filter %>%
  mutate(stem = stem_words(word, language = "porter")) %>%
  filter(nchar(stem) > 2) %>%
  group_by(query, stem) %>%
  summarize(n = sum(n)) %>%
  ungroup()

saveRDS(queries_test_stem_filter, "imported/queries_test_stem_filter.rds")

# queries_test_stem_filter %>%
#   arrange(desc(n))

queries_test_lemma_filter <- queries_test_word_filter %>%
  mutate(lemma = lemmatize_words(word)) %>%
  filter(nchar(lemma) > 2) %>%
  group_by(query, lemma) %>%
  summarize(n = sum(n)) %>%
  ungroup()

saveRDS(queries_test_lemma_filter, "imported/queries_test_lemma_filter.rds")

queries_test_lemma_filter %>%
  arrange(desc(n)) 



