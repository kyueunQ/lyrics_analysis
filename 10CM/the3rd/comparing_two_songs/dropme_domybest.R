library(dplyr)

# 10CM, '어제 너는 나를 버렸어' 가사 불러오기
raw_dropme <- readLines("10CM_어제너는나를버렸어.txt", encoding = "UTF-8")
dropme <- raw_dropme %>% 
  as_tibble() %>% 
  mutate(title = "dropme")

# 10CM, '열심히 할게' 가사 불러오기
raw_domybest <- readLines("10CM_열심히할게.txt", encoding = "UTF-8")
domybest <- raw_domybest %>% 
  as_tibble() %>% 
  mutate(title = "domybest")

# 두 개의 노래 가사 붙이기 
bind_lyrics <- bind_rows(dropme, domybest) %>% 
  select(title, value)

head(bind_lyrics)
tail(bind_lyrics)

# pre-processing
library(stringr)
lyrics <- bind_lyrics %>% 
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
lyrics

# token
library(tidytext)
library(KoNLP)
lyrics <- lyrics %>% 
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
lyrics

# count frequnecy 
frequency <- lyrics %>% 
  count(title, word)

# limit word length : over 2 letters
frequency_limit <- lyrics %>% 
  count(title, word) %>% 
  filter(str_count(word) > 1)

head(frequency)
head(frequency_limit)


# extract top 10 frequntly using words
# A tibble: 38 x 3
top10 <- frequency_limit %>% 
  group_by(title) %>% 
  slice_max(n, n = 10)
top10

# confirm the result
# no limitation in words' length
top10_2 <- frequency %>% 
  group_by(title) %>% 
  slice_max(n, n = 10)
top10_2

# "어제 너는 나를 버렸어" 노래  
# A tibble: 10 x 3  (빈도 동점인 행 없음)
top10 %>% 
  filter(title == "dropme")

# "열심히할게" 노래 
# A tibble: 28 x 3 (빈도 동점인 행 많음)
top10 %>% 
  filter(title == "domybest")


# 빈도 동점인 건들 제거 
# 오로지 두 노래 각각의 top 10 추출 
# A tibble: 20 x 3
top10_result <- frequency_limit %>% 
  group_by(title) %>% 
  slice_max(n, n = 10, with_ties = F)
top10_result


# bar graph
library(ggplot2)
ggplot(top10_result, aes(x = reorder_within(word, n, title),
                         y = n,
                         fill = title)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ title,
           scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))
