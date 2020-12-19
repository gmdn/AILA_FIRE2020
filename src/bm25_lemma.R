######## lemma BM25 #########

### casedocs

df_lemma <- casedocs_lemma_filter %>%
  select(doc_id, lemma) %>%
  group_by(lemma) %>%
  summarise(df = n()) %>%
  #filter(df < nrow(casedocs_split) * 0.4)
  filter(df > 1)
  #count(word, df, sort = TRUE)
df_lemma

total_lemma <- casedocs_lemma_filter %>%
  group_by(doc_id) %>%
  summarise(total = sum(n))

k1 <- 1.2
b <- 0.75
avg_doc_length <- round(mean(total_lemma$total))

casedocs_lemma_bm25 <- casedocs_lemma_filter %>%
  inner_join(total_lemma) %>%
  inner_join(df_lemma) %>%
  mutate(tf = n / total) %>%
  mutate(idf = log(1 + 1 / df)) %>%
  mutate(bm25_score = idf * (tf * (k1 + 1)) / (tf + k1 * (1 - b + b * total / avg_doc_length)))
casedocs_lemma_bm25




### statutes

df_lemma <- statutes_lemma_filter %>%
  select(doc_id, lemma) %>%
  group_by(lemma) %>%
  summarise(df = n()) %>%
  #filter(df < nrow(statutes_split) * 0.4)
  filter(df > 1)
#count(word, df, sort = TRUE)
df_lemma

total_lemma <- statutes_lemma_filter %>%
  group_by(doc_id) %>%
  summarise(total = sum(n))

k1 <- 1.2
b <- 0.75
avg_doc_length <- round(mean(total_lemma$total))

statutes_lemma_bm25 <- statutes_lemma_filter %>%
  inner_join(total_lemma) %>%
  inner_join(df_lemma) %>%
  mutate(tf = n / total) %>%
  mutate(idf = log(1 + 1 / df)) %>%
  mutate(bm25_score = idf * (tf * (k1 + 1)) / (tf + k1 * (1 - b + b * total / avg_doc_length)))
statutes_lemma_bm25



## queries
# queries borrow idf from dataset
total_lemma <- queries_train_lemma_filter %>%
  group_by(query) %>%
  summarise(total = sum(n))

queries_train_lemma_tf_idf <- queries_train_lemma_filter %>%
  inner_join(total_lemma) %>%
  mutate(tf = n / total) %>%
  inner_join(distinct(select(casedocs_lemma_tf_idf, lemma, idf))) %>%
  mutate(tf_idf = tf * idf) 
queries_train_lemma_tf_idf



## queries
# queries borrow idf from dataset
total_lemma <- queries_test_lemma_filter %>%
  group_by(query) %>%
  summarise(total = sum(n))

queries_test_lemma_tf_idf <- queries_test_lemma_filter %>%
  inner_join(total_lemma) %>%
  mutate(tf = n / total) %>%
  inner_join(distinct(select(casedocs_lemma_tf_idf, lemma, idf))) %>%
  mutate(tf_idf = tf * idf) 
queries_test_lemma_tf_idf
