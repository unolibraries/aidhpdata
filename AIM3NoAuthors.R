##Program to do Topic modeling on American Indian magazine
##Third draft, changes: use trigrams for tf_idf

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
aim_trigrams <- tibble %>% 
  mutate(line = row_number()) %>%
  tidytext::unnest_tokens(trigram, text, token = "ngrams", n = 3)

aim_trigrams

trigrams_separated <- aim_trigrams %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") 

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(is.na(as.numeric(word1))) %>%
  filter(is.na(as.numeric(word2))) %>%
  filter(is.na(as.numeric(word3))) %>%
  filter(word1 != "american") %>%
  filter(word2 != "indian") %>%
  filter(word1 != "stitute") %>%
  filter(word1 != "tute") %>%
  filter(word1 != "nthe") %>%
  filter(word1 != "indian") %>%
  # filter(word1 != "quarterly") %>%
  filter(word2 !=  "quarterly") %>%
  filter(word3 != "coolidge") %>%
  filter(word2 != "coolidge") %>%
  filter(word3 != "cloud") %>%
  filter(word3 != "morgan") %>%
  filter(word2 != "zitkala") %>%
  filter(word3 != "zitkala") %>%
  filter(word2 != "bonnin") %>%
  filter(word3 != "bonnin") %>%
  filter(word3 != "montezuma") %>%
  filter(word2 != "henry") %>%
  filter(word3 != "gertrude") %>%
  filter(word1 != "indian")

trigram_counts <- trigrams_filtered %>%
  count(word1, word2, word3, sort = TRUE)

trigram_counts

trigrams_united <- trigrams_filtered %>%
  unite(trigram, word1, word2, word3, sep = " ")

trigrams_united

trigram_tf_idf <- trigrams_united %>%
  count(title, trigram) %>%
  bind_tf_idf(trigram, title, n) %>%
  arrange(desc(tf_idf)) %>%
  top_n(50)

trigram_tf_idf

trigram_tf_idf %>%
  mutate(trigram = reorder_within(trigram, tf_idf, title)) %>%
  ggplot(aes(trigram, tf_idf, fill = title)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ title, scales = "free", ncol = 3) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text=element_text(size=5))

aim_dfm <- trigrams_united %>%
  count(title, trigram, sort = TRUE) %>%
  cast_dfm(title, trigram, n)

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
       title = "Highest trigram probabilities for each topic",
       subtitle = "Different trigrams are associated with different topics")

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

