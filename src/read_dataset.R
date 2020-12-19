library(readr)
library(dplyr)

queries_train <- read_delim("../dataset/Query_doc.txt", delim = "|", col_names = FALSE)

queries_train <- queries_train %>%
  select(X1, X3) %>%
  rename(query = X1, text = X3)

saveRDS(queries_train, "./imported/queries_train.rds")


queries_test <- read_delim("../../aila20-test/TestData_release/Task1_test_data.txt", delim = "|", col_names = FALSE)

queries_test <- queries_test %>%
  select(X1, X3) %>%
  rename(query = X1, text = X3)

saveRDS(queries_test, "./imported/queries_test.rds")


qrels_casesdocs_train <- read_delim("../relevance_judgements/task1a_rel_judgements.txt", 
                              delim = " ", col_names = FALSE) %>%
  rename(query = X1, Q0 = X2, doc = X3, relevance = X4)

saveRDS(qrels_casesdocs_train, "./imported/qrels_casedocs_train.rds")

qrels_statutes_train <- read_delim("../relevance_judgements/task1b_rel_judgements.txt", 
                             delim = " ", col_names = FALSE) %>%
  rename(query = X1, Q0 = X2, doc = X3, relevance = X4)

saveRDS(qrels_statutes_train, "./imported/qrels_statutes_train.rds")

# qrels_casesdocs %>%
#   filter(relevance == 1) %>%
#   arrange(query, doc)
# 
# qrels_statutes %>%
#   filter(relevance == 1) %>%
#   arrange(query, doc)


library(readtext)
library(tibble)
library(tidyr)
library(stringr)

path_casedocs <- "../dataset/Object_casedocs/"
file_casedocs <- list.files(path_casedocs)

casedocs <- readtext::readtext(paste0(path_casedocs, file_casedocs)) %>%
  as_tibble()

casedocs_split <- separate(data = casedocs, 
                     col = text, 
                     into = c("who", "where", "nothing", "when", "what", "delivered", "text"),
                     sep = "\n", 
                     extra = "merge") %>%
  select(-nothing) %>%
  mutate(doc_id = str_remove(doc_id, "\\.txt"))

saveRDS(casedocs_split, "./imported/casedocs_split.rds")

path_statutes <- "../dataset/Object_statutes/"
file_statutes <- list.files(path_statutes)

statutes <- readtext::readtext(paste0(path_statutes, file_statutes)) %>%
  as_tibble()

statutes_split <- separate(data = statutes,
                          col = text,
                          into = c("title", "description"),
                          sep = "\n") %>%
  mutate(doc_id = str_remove(doc_id, "\\.txt"), 
         title = str_remove(title, "Title:"),
         description = str_remove(description, "Desc:")) %>%
  mutate(full_text = paste0(title, description))

saveRDS(statutes_split, "./imported/statutes_split.rds")
