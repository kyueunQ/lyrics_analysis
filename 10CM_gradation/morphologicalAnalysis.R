# morphological analysis
# prepare using 'NoNLP' 
install.packages("multilinguer")
library(multilinguer)
install_jdk()

install.packages(c("stringr", "hash", "tau", "Sejong", "RSQLite", "devtools"), type = "binary")


install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP",
                        upgrade = "never",
                        INSTALL_opts = c("--no-multiarch"))
library(KoNLP)

useNIADic()

install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages("tidytext")
library(tidytext)

# read the file
raw_lyrics <- readLines("Gradation_10CM.txt", encoding = "UTF-8")

raw_lyrics

# data preprocessing
gradation <- raw_lyrics %>% 
  str_replace_all("[^가-힣]", " ") %>% 
  str_squish() %>% 
  as_tibble()


library(KoNLP)
# morphological analysis
word_noun <- gradation %>% 
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)

word_noun <- word_noun %>% 
  count(word, sort = T) %>% 
  filter(str_count(word) > 1)

word_noun

top20 <- word_noun %>% 
  head(20)

install.packages("showtext")
library(showtext)

font_add_google(name = "Gamja Flower", family = "gamjaflower")
showtext.auto()

# bar graph
library(ggplot2)
ggplot(top20, aes(x = reorder(word,n), y = n)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = n), hjust = -0.5) +
  labs(title = "10CM '그라데이션'에서 가장 많이 나온 낱말",
       x = NULL, y = NULL) +
  labs(x = NULL) +
  theme(title = element_text(size = 20),
        text = element_text(family = "gamjaflower"))

# word cloud
library(showtext)
font_add_google(name = "Black Han Sans", family = "blackhansans")
showtext_auto()

library(ggwordcloud)
ggplot(word_noun, aes(label = word, size = n, col = n)) +
  geom_text_wordcloud(seed = 1234, family = "blackhansans") +
                        scale_radius(limits = c(1, NA),
                                     range = c(5, 20)) +
                        scale_color_gradient(low = "#F08080", high = "#B22222") +
                        theme_minimal()
