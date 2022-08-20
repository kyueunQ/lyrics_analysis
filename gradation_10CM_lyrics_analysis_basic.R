# 그라데이션 - 10CM 가사 분석 (1) 

txt <- readLines("Gradation_10CM.txt", encoding = "UTF-8")

head(txt)

# data preprocessing
install.packages("stringr")
library(stringr)

gradation <- txt %>% 
  str_replace_all("[^가-힣]", " ") %>% 
  str_squish() %>% 
  as_tibble()

# token : words
install.packages("tidytext")
library(tidytext)

word_space <- gradation %>% 
  unnest_tokens(input = value,
                output = word,
                token = "words")

word_space <- word_space %>% 
  count(word, sort = T)

word_space <- word_space %>% 
  head(20)

# google fonts
install.packages("showtext")
library(showtext)

font_add_google(name = "Gamja Flower", family = "gamjaflower")
showtext.auto()

# bar graph
install.packages("ggplot2")
library(ggplot2)

ggplot(word_space, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  coord_flip() + 
  geom_text(aes(label = n), hjust = -0.5) +
  labs(title = "10CM 그라데이션 흥해라♡ ", 
       x = NULL, y = NULL) +
  theme(title = element_text(size = 20),
        text = element_text(family = "gamjaflower"))

ggplot(word_space, aes(label = word,
                       size = n,
                       col = n)) +
  geom_text_wordcloud(seed = 1234,
                      family = "blackhansans") +
  scale_radius

# wordcloud
install.packages("ggwordcloud")
library(ggwordcloud)

ggplot(word_space, aes(label = word,
                       size = n,
                       col = n)) +
  geom_text_wordcloud(seed = 1234,
                      family = "gamjaflower") +
  scale_color_gradient(low = "#DC143C",
                       high = "#CD5C5C") +
  theme_minimal()

