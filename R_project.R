#options(repos = c(CRAN = "https://ftp.fau.de/cran/"))
library(ggplot2)
library(tidyverse)
library(tidyselect)
library(readr)
library(tidytext)
library(ggrepel)
library(viridisLite)

play <- read_csv("author_words_df.csv")
play

tidy_play<-play %>% #токенизируем еще раз для удобной обработки
  mutate(words=str_remove_all(words, "^\\[|\\]$")) %>%
  mutate(words=str_remove_all(words, "'")) %>%
  separate_rows(words, sep=",\\s*") 


#Способ визуализации: Bubble chart

#Расчёт TF-IDF 
tidy_play<-ungroup(tidy_play)
tf_idf<-tidy_play %>%
  group_by(author, words) %>%
  summarise(n=n(), .groups="drop") %>%
  bind_tf_idf(words, author, n) %>%
  arrange(desc(tf_idf))

#Определяем общие слова (встречаются мин у 2)
common_words <- tfidf %>%
  group_by(words) %>%
  summarise(df = n()) %>%
  filter(df >= 2) %>%
  pull(words)

#Добавляем тип слова
tfidf <- tfidf %>%
  mutate(word_type = ifelse(words %in% common_words, "common", "unique"))

#Берём топ-15 слов по TF-IDF для каждого автора
top_terms <- tfidf %>%
  group_by(author) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup()

#бабл чарт график через ggplot
best <- ggplot(top_terms, aes(x = reorder_within(words, tf_idf, author), 
                      y = tf_idf, 
                      size = tf_idf,
                      color = author,
                      shape = word_type)) +
  geom_point(alpha = 0.8) +
  scale_shape_manual(values = c("common" = 1, "unique" = 19), 
                     name = "Type of word",
                     labels = c("Common (*min 2 authors have it)", "Unique")) +
  scale_size(range = c(2, 10), name = "TF-IDF index") +
  scale_color_brewer(palette="Dark2", name="Author") +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~author, scales = "free_y", ncol = 2) +
  theme_minimal(base_size = 13) +
  labs(title = "Lexical diversity",
       subtitle = "Unique VS Common",
       x = NULL, y = "TF-IDF",
       color="Author") +
  theme(
    panel.background = element_rect(fill="white"),
    plot.title=element_text(size=20, face="bold", family="Arial"),
    plot.subtitle = element_text(size=16, face="italic", family="Arial"),
    axis.text.x=element_text(size=10, family="Arial"),
    axis.text.y=element_text(size=10, family="Arial"),
    strip.text=element_text(size=14, face="italic", family="Arial"),
    legend.title=element_text(size=16, face="bold", family="Arial"),
    legend.text=element_text(size=14, face="italic", family="Arial")
  )
    
best

