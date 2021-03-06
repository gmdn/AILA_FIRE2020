######## stem #########

######## casedocs #########

df_stem <- casedocs_stem_filter %>%
  select(doc_id, stem) %>%
  group_by(stem) %>%
  summarise(df = n())
#count(word, df, sort = TRUE)
df_stem

total_stem <- casedocs_stem_filter %>%
  group_by(doc_id) %>%
  summarise(total = sum(n))

casedocs_stem_tf_idf <- casedocs_stem_filter %>%
  inner_join(total_stem) %>%
  inner_join(df_stem) %>%
  mutate(tf = n / total) %>%
  mutate(idf = log(1 + 1 / df)) %>%
  mutate(tf_idf = tf * idf) 
casedocs_stem_tf_idf

# total_tfidf <- casedocs_stem_tf_idf %>%
#   group_by(doc_id) %>%
#   summarise(sum_tfidf = sum(tf_idf))
# 
# casedocs_stem_norm_tf_idf <- casedocs_stem_tf_idf %>%
#   inner_join(total_tfidf) %>%
#   mutate(norm_tf_idf = tf_idf / sum_tfidf)

casedocs_stem_tf_idf_norm_doc <- casedocs_stem_tf_idf%>%
  group_by(doc_id) %>%
  summarise(norm_doc = sqrt(sum(tf_idf^2)))

casedocs_stem_tf_idf_norm <- casedocs_stem_tf_idf %>%
  inner_join(casedocs_stem_tf_idf_norm_doc) %>%
  select(doc_id, stem, tf_idf, norm_doc)
casedocs_stem_tf_idf_norm


######## statutes #########

df_stem <- statutes_stem_filter %>%
  select(doc_id, stem) %>%
  group_by(stem) %>%
  summarise(df = n())
#count(word, df, sort = TRUE)
df_stem

total_stem <- statutes_stem_filter %>%
  group_by(doc_id) %>%
  summarise(total = sum(n))

statutes_stem_tf_idf <- statutes_stem_filter %>%
  inner_join(total_stem) %>%
  inner_join(df_stem) %>%
  mutate(tf = n / total) %>%
  mutate(idf = log(1 + 1 / df)) %>%
  mutate(tf_idf = tf * idf) 
statutes_stem_tf_idf

# total_tfidf <- statutes_stem_tf_idf %>%
#   group_by(doc_id) %>%
#   summarise(sum_tfidf = sum(tf_idf))
# 
# statutes_stem_norm_tf_idf <- statutes_stem_tf_idf %>%
#   inner_join(total_tfidf) %>%
#   mutate(norm_tf_idf = tf_idf / sum_tfidf)

statutes_stem_tf_idf_norm_doc <- statutes_stem_tf_idf%>%
  group_by(doc_id) %>%
  summarise(norm_doc = sqrt(sum(tf_idf^2)))

statutes_stem_tf_idf_norm <- statutes_stem_tf_idf %>%
  inner_join(statutes_stem_tf_idf_norm_doc) %>%
  select(doc_id, stem, tf_idf, norm_doc)
statutes_stem_tf_idf_norm

######## queries #########

## queries
# queries borrow idf from dataset
total_stem <- queries_train_stem_filter %>%
  group_by(query) %>%
  summarise(total = sum(n))

queries_train_stem_tf_idf <- queries_train_stem_filter %>%
  inner_join(total_stem) %>%
  mutate(tf = n / total) %>%
  inner_join(distinct(select(casedocs_stem_tf_idf, stem, idf))) %>%
  mutate(tf_idf = tf * idf) 
queries_train_stem_tf_idf

# total_tfidf <- queries_stem_tf_idf %>%
#   group_by(query) %>%
#   summarise(sum_tfidf = sum(tf_idf))
# 
# queries_stem_norm_tf_idf <- queries_stem_tf_idf %>%
#   inner_join(total_tfidf) %>%
#   mutate(norm_tf_idf_q = tf_idf / sum_tfidf)

queries_train_stem_norm_q <- queries_train_stem_tf_idf%>%
  group_by(query) %>%
  summarise(norm_q = sqrt(sum(tf_idf^2)))
queries_train_stem_norm_q

queries_train_stem_tf_idf_norm <- queries_train_stem_norm_q %>%
  inner_join(queries_train_stem_tf_idf) %>%
  select(query, stem, tf_idf, norm_q)

queries_train_stem_tf_idf_norm %>%
  arrange(query) 



## queries
# queries borrow idf from dataset
total_stem <- queries_test_stem_filter %>%
  group_by(query) %>%
  summarise(total = sum(n))

queries_test_stem_tf_idf <- queries_test_stem_filter %>%
  inner_join(total_stem) %>%
  mutate(tf = n / total) %>%
  inner_join(distinct(select(casedocs_stem_tf_idf, stem, idf))) %>%
  mutate(tf_idf = tf * idf) 
queries_test_stem_tf_idf

# total_tfidf <- queries_stem_tf_idf %>%
#   group_by(query) %>%
#   summarise(sum_tfidf = sum(tf_idf))
# 
# queries_stem_norm_tf_idf <- queries_stem_tf_idf %>%
#   inner_join(total_tfidf) %>%
#   mutate(norm_tf_idf_q = tf_idf / sum_tfidf)

queries_test_stem_norm_q <- queries_test_stem_tf_idf%>%
  group_by(query) %>%
  summarise(norm_q = sqrt(sum(tf_idf^2)))
queries_test_stem_norm_q

queries_test_stem_tf_idf_norm <- queries_test_stem_norm_q %>%
  inner_join(queries_test_stem_tf_idf) %>%
  select(query, stem, tf_idf, norm_q)

queries_test_stem_tf_idf_norm %>%
  arrange(query) 

