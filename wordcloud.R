library(wordcloud)
library(rwhatsapp)
library(tidytext)
library(dplyr)
library(stopwords)

# The purpose of this code is to create a wordcloud based 
# on an author's words. 

data(stop_words)

data_path <- base::readline(prompt = "Enter your data path: ")
data =rwhatsapp::rwa_read("chat.txt")

#Preparing stopwords
to_remove = c(stopwords::stopwords(language = "en"),
              stopwords::stopwords(language = "es"),
              'media','omitted',"https","mmm")

author_name = base::readline(prompt="Enter author name: ")

data %>% tidytext::unnest_tokens(word,text) %>% filter(author==author_name) %>%
        filter(!word %in% to_remove) %>%
        dplyr::anti_join(stop_words,copy = FALSE) %>%
        count(word) %>% with(wordcloud(word,n,max.words=40))

