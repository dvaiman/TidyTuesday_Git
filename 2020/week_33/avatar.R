# TidyTuesday
# week 33 - Avatar The last airbender
# Grapichs: Daniel Väisänen
library(here)
library(tidyverse)
library(tvthemes)
library(ggwordcloud) 
library(cowplot)
library(tidytext)



avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv')

afinn <- get_sentiments('afinn')#runs between -5 and 5, with negative scores indicating negative sentiment and positive scores indicating positive sentiment
nrc <- get_sentiments('nrc') # positive, negative, anger, anticipation, disgust, fear, joy, sadness, surprise, and trust
bing <- get_sentiments('bing')# binary fashion into positive and negative categories

# sentiment analysis
a<-avatar %>% filter(character == "Aang") %>% 
  group_by(character) %>%
  unnest_tokens(word, character_words) %>% 
  inner_join(bing) %>%
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n))


# ggwordcloud plot

set.seed(81)
p1<- a %>% select(word, n, sentiment) %>% rename( "freq" =n ) %>% 
  mutate(word= as.character(word)) %>% 
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1))) %>% 
ggplot(aes(label = word, size = freq, color = sentiment)) +
  geom_text_wordcloud_area(aes(angle= angle), shape = "square", family = "Slayer") +
  scale_size_area(max_size = 18) +
  scale_fill_avatar() +
  scale_color_avatar(palette = "AirNomads") +
  labs(title = "\n\nthe last airbender", 
       subtitle = "Sentiment analysis of positively and negatively \nconnoted words spoken by Aang",
       caption = "Source: appa | Graphics: Daniel Vaisanen     ") +
  theme_avatar(text.font = "Slayer") +
theme(plot.title = element_text(size = 25, hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      plot.margin = margin(1, 1, 1, 1, "cm")) +
  coord_cartesian(clip = "off")


# cowplot
plot_grid(p1) +
  draw_image(here("2020/week_33/aang2.png"), scale = .12, x = 0, y = 0.42) 


ggsave(here("2020/week_33/avatar.png"), dpi = 300, width = 8, height = 8)
