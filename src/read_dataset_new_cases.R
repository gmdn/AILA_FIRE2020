library(readr)
library(dplyr)
library(readtext)
library(tibble)
library(tidyr)
library(stringr)

# On September 23rd, the organizers sent an email with a new set of casedocs.

path_casedocs <- "../dataset/Object_casedocs_2020/"
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

saveRDS(casedocs_split, "./imported_2020/casedocs_split.rds")
