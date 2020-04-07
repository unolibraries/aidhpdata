#program for topic modeling on .pdf files of American Indian Magazine
#first draft: k = ?
library(magrittr)
library(dplyr)
library(tm)
library(tidyselect)
library(tidytext)
library(quanteda)
library(stm)
library(ggplot2)
library(tidyverse)
library(pdftools)

file_names <- list.files()
head(file_names, 24)

text_files <- lapply(file_names, pdf_text)

docs = ""
for(i in 1:length(file_names)) {
  docs = c(docs, paste(text_files[i], collapse = "\n"))
}
docs2 = docs[2:25]

length(docs2)

df <- data.frame(title = file_names, text = docs2)
tibble <- as_tibble(df)
tibble <- mutate(tibble, text = as.character(text))
tibble
tidy_aim <- tibble %>%
  mutate(line = row_number()) %>%
  tidytext::unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word != "Indian") %>%
  filter(word != "indian") %>%
  filter(word != "indians") %>%
  filter(word != "nthe") %>%
  filter(is.na(as.numeric(word))) %>%
  filter(word != "nand") %>%
  filter(word != "nof") %>%
  filter(word != "society") %>%
  filter(word != "american") %>%
  filter(word != "race") %>%
  filter(word != "white") %>%
  filter(word != "people") %>%
  filter(word != "school") %>%
  filter(word != "government") %>%
  filter(word != "time") %>%
  filter(word != "life") %>%
  filter(word != "day") %>%
  filter(word != "country") %>%
  filter(word != "land")  %>%
  filter(word != "journal") %>%
  filter(word != "nto") %>%
  filter(word != "schools") %>%
  filter(word != "quarterly") %>%
  filter(word != "red") %>%
  filter(word != "reservation") %>%
  filter(word != "con") %>%
  filter(word != "children") %>%
  filter(word != "nin") %>%
  filter(word != "conference") %>%
  filter(word != "civilization") %>%
  filter(word != "conference") %>%
  filter(word != "er") %>%
  filter(word != "ed") %>%
  filter(word != "en") %>%
  filter(word != "em") %>%
  filter(word != "ey") %>%
  filter(word != "st") %>%
  filter(word != "dr") %>%
  filter(word != "fo") %>%
  filter(word != "ent") %>%
  filter(word != "ing") %>%
  filter(word != "es") %>%
  filter(word != "na") %>%
  filter(word != "di") %>%
  filter(word != "nthat") %>%
  filter(word != "nis") %>%
  filter(word != "nindian") %>%
  filter(word != "united") 
  
tidy_aim

tidy_aim %>%
  count(word, sort = TRUE)

aim_tf_idf <- tidy_aim %>%
  count(title, word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(-tf_idf) %>%
  group_by(title) %>%
  top_n(10) %>%
  ungroup

aim_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, title)) %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ title, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=5))

aim_dfm <- tidy_aim %>%
  count(title, word, sort = TRUE) %>%
  cast_dfm(title, word, n)

topic_model <- stm(aim_dfm, K = 20, verbose = FALSE, init.type = "Spectral")

summary(topic_model)

td_beta <- tidy(topic_model)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

td_gamma <- tidy(topic_model, matrix = "gamma", document_names = rownames(aim_dfm))

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       y = "Number of texts", x = expression(gamma))

library(scales)
td_gamma %>% 
  mutate(document = factor(document, levels = rev(unique(document)))) %>%
  group_by(document) %>%
  top_n(1) %>%
  ungroup %>%
  ggplot(aes(document, gamma, label = document, fill = as.factor(topic))) +
  geom_col() +
  geom_text(aes(document, 0.01), hjust = 0,
            color = "white", size = 2.5) +
  scale_fill_manual(values = c("#F48024", "#0077CC", "#5FBA7D", 
                               "#8C60A7", "#34495E", "#CDDC39", 
                               "#FF33FB", "#9EF7FF", "#B66ADE", 
                               "#F2B25C", "#71DF53", "#F18E4E", 
                               "#898282", "#F9DB09", "#0610BA",
                               "#593079", "#FB21B9", "#1FC19E", 
                               "#76A623", "#66145C")) +
  scale_y_continuous(expand = c(0,0),
                     labels = percent_format()) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text.y=element_blank()) +
  labs(x = NULL, y = expression(gamma), fill = "Topic")    

