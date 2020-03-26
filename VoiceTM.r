##Reads text into program from pdf files
library(magrittr)
library(dplyr)
library(tm)
library(tidyselect)
library(tidytext)
library(quanteda)
library(stm)
library(ggplot2)
library(tidyverse)

file_names <- list.files()
head(file_names, 17)

docs = ""
for(i in 1:length(file_names)) {
  docs = c(docs, paste(readLines(file(file_names[i])), collapse = "\n"))
}
docs2 = docs[2:18]

df <- data.frame(title = file_names, text = docs2)
tibble <- as_tibble(df)
tibble <- mutate(tibble, text = as.character(text))
tibble
tidy_voice <- tibble %>%
  mutate(line = row_number()) %>%
  tidytext::unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  filter(word != "Indian") %>%
  filter(word != "indian") %>%
  filter(word != "indians") %>%
  filter(word != "people") %>%
  filter(is.na(as.numeric(word))) %>%
  filter(word != "voice")

tidy_voice

tidy_voice %>%
  count(word, sort = TRUE)

voice_tf_idf <- tidy_voice %>%
  count(title, word, sort = TRUE) %>%
  bind_tf_idf(word, title, n) %>%
  arrange(-tf_idf) %>%
  group_by(title) %>%
  top_n(10) %>%
  ungroup

voice_tf_idf %>%
  mutate(word = reorder_within(word, tf_idf, title)) %>%
  ggplot(aes(word, tf_idf, fill = title)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ title, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=5))

voice_dfm <- tidy_voice %>%
  count(title, word, sort = TRUE) %>%
  cast_dfm(title, word, n)

topic_model <- stm(voice_dfm, K = 8, verbose = FALSE, init.type = "Spectral")

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

td_gamma <- tidy(topic_model, matrix = "gamma", document_names = rownames(voice_dfm))

  ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 stories",
       y = "Number of stories", x = expression(gamma))
  
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
                                 "#8C60A7", "#34495E", "#CDDC39", "#FF33FB", "#9EF7FF")) +
    scale_y_continuous(expand = c(0,0),
                       labels = percent_format()) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y=element_blank()) +
    labs(x = NULL, y = expression(gamma), fill = "Topic")    

