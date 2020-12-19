######## lemma #########

############# CASEDOCS #################

df_lemma <- casedocs_lemma_filter %>%
  anti_join(get_stopwords(source = "smart"), by = c("lemma" = "word")) %>%
  anti_join(get_stopwords(source = "stopwords-iso"), by = c("lemma" = "word")) %>%
  select(doc_id, lemma) %>%
  group_by(lemma) %>%
  summarise(df = n()) #%>%
  #filter(df < nrow(casedocs_split) * 0.4)
  #filter(df > 1)
#count(word, df, sort = TRUE)

total_lemma <- casedocs_lemma_filter %>%
  group_by(doc_id) %>%
  summarise(total = sum(n))

casedocs_lemma_tf_idf <- casedocs_lemma_filter %>%
  inner_join(total_lemma) %>%
  inner_join(df_lemma) %>%
  mutate(tf = n / total) %>%
  mutate(idf = log(1 + 1 / df)) %>%
  mutate(tf_idf = tf * idf) 
casedocs_lemma_tf_idf

casedocs_lemma_tf_idf_norm_doc <- casedocs_lemma_tf_idf %>%
  group_by(doc_id) %>%
  summarise(norm_doc = sqrt(sum(tf_idf^2)))

casedocs_lemma_tf_idf_norm <- casedocs_lemma_tf_idf %>%
  inner_join(casedocs_lemma_tf_idf_norm_doc) %>%
  select(doc_id, lemma, tf_idf, norm_doc)
casedocs_lemma_tf_idf_norm


############ STATUTES #############

df_lemma <- statutes_lemma_filter %>%
  anti_join(get_stopwords(source = "smart"), by = c("lemma" = "word")) %>%
  anti_join(get_stopwords(source = "stopwords-iso"), by = c("lemma" = "word")) %>%
  select(doc_id, lemma) %>%
  group_by(lemma) %>%
  summarise(df = n()) #%>%
#filter(df < nrow(statutes_split) * 0.4)
#filter(df > 1)
#count(word, df, sort = TRUE)

total_lemma <- statutes_lemma_filter %>%
  group_by(doc_id) %>%
  summarise(total = sum(n))

statutes_lemma_tf_idf <- statutes_lemma_filter %>%
  inner_join(total_lemma) %>%
  inner_join(df_lemma) %>%
  mutate(tf = n / total) %>%
  mutate(idf = log(1 + 1 / df)) %>%
  mutate(tf_idf = tf * idf) 
statutes_lemma_tf_idf

statutes_lemma_tf_idf_norm_doc <- statutes_lemma_tf_idf%>%
  group_by(doc_id) %>%
  summarise(norm_doc = sqrt(sum(tf_idf^2)))

statutes_lemma_tf_idf_norm <- statutes_lemma_tf_idf %>%
  inner_join(statutes_lemma_tf_idf_norm_doc) %>%
  select(doc_id, lemma, tf_idf, norm_doc)
statutes_lemma_tf_idf_norm


############ QUERIES #############


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

# total_tfidf <- queries_lemma_tf_idf %>%
#   group_by(query) %>%
#   summarise(sum_tfidf = sum(tf_idf))
# 
# queries_lemma_norm_tf_idf <- queries_lemma_tf_idf %>%
#   inner_join(total_tfidf) %>%
#   mutate(norm_tf_idf_q = tf_idf / sum_tfidf)

queries_train_lemma_norm_q <- queries_train_lemma_tf_idf%>%
  group_by(query) %>%
  summarise(norm_q = sqrt(sum(tf_idf^2)))
queries_train_lemma_norm_q

queries_train_lemma_tf_idf_norm <- queries_train_lemma_norm_q %>%
  inner_join(queries_train_lemma_tf_idf) %>%
  select(query, lemma, idf, tf_idf, norm_q)

queries_train_lemma_tf_idf_norm %>%
  arrange(query) 



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

# total_tfidf <- queries_lemma_tf_idf %>%
#   group_by(query) %>%
#   summarise(sum_tfidf = sum(tf_idf))
# 
# queries_lemma_norm_tf_idf <- queries_lemma_tf_idf %>%
#   inner_join(total_tfidf) %>%
#   mutate(norm_tf_idf_q = tf_idf / sum_tfidf)

queries_test_lemma_norm_q <- queries_test_lemma_tf_idf%>%
  group_by(query) %>%
  summarise(norm_q = sqrt(sum(tf_idf^2)))
queries_test_lemma_norm_q

queries_test_lemma_tf_idf_norm <- queries_test_lemma_norm_q %>%
  inner_join(queries_test_lemma_tf_idf) %>%
  select(query, lemma, idf, tf_idf, norm_q)

queries_test_lemma_tf_idf_norm %>%
  arrange(query) 
