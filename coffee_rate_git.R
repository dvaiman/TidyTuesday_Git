
# Coffee ratings around the worls

# Libraries
library(tidyverse)
library(ggthemes)
library(ggtext)



coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

# rename countries
coffee_ratings$country_of_origin[coffee_ratings$country_of_origin == "United States (Puerto Rico)"] <- 'United States'
coffee_ratings$country_of_origin[coffee_ratings$country_of_origin == "United States (Hawaii)"] <- 'United States'
coffee_ratings$country_of_origin[coffee_ratings$country_of_origin == "Tanzania, United Republic Of"] <- 'Tanzania'
coffee_ratings$country_of_origin[coffee_ratings$country_of_origin == "Cote d?Ivoire"] <- 'Cote d\'Ivoire'

coffee <- coffee_ratings %>% 
  select(total_cup_points, species, country_of_origin, aroma:moisture) %>% 
  filter(!country_of_origin == "Honduras") %>% 
  # mutate into z-scores
  mutate(across(c(aroma:moisture, total_cup_points ), ~scale(.x))) %>%  
  group_by(country_of_origin) %>% 
  summarise(total_cup_points = mean(total_cup_points), count = n()) %>% 
  mutate(country_of_origin1 = ifelse(total_cup_points < 0, NA, country_of_origin)) %>% 
  mutate(country_of_origin2 = ifelse(total_cup_points > 0, NA, country_of_origin)) %>% 
  mutate(country_of_origin = fct_reorder(country_of_origin, total_cup_points))  %>% 
  # Plot
  ggplot(aes(x = total_cup_points, y =country_of_origin)) + 
  geom_point(aes(size = count, color= total_cup_points > 0), shape = 18) +
  geom_segment(aes( xend =0, yend=country_of_origin, color= total_cup_points > 0),size=0.5) +
  geom_text(aes(x= 0, label= country_of_origin2), angle = 50, nudge_x = 0.02, hjust = 0, color="grey", size=3) +
  geom_text(aes(x= 0, label= country_of_origin1), angle = 50, nudge_x = -0.02, hjust = 1, color="grey", size=3) +
  coord_flip(clip = "off") +
  theme_wsj() +
  scale_color_manual(values = c("goldenrod3", "#76EEC6")) +
  annotate("text", x = -1.4, y = 7,  size = 3, color = "grey50",
           label = "Z score: -1.9") +
  annotate("text", x = 1.2, y = 27,  size = 3, color = "gray50",
           label = "Z score: 1.3") +
  annotate("text", x = 1, y = 11.5,  size = 3, color = "grey",
           label = "Larger head; more tastings") +
  geom_curve(aes(x = -1.93, y = 1.2, xend = -1.5, yend = 7),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.4,
             color = "gray50", curvature = 0.4) +
  geom_curve(aes(x = 1.4, y = 33, xend = 1.27, yend = 27),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.1,
             color = "gray50", curvature = 0.4) +
  geom_curve(aes(x = 0.45, y = 27, xend = 1.02, yend = 14.5),
             arrow = arrow(length = unit(0.07, "inch")), size = 0.2,
             color = "darkgrey", curvature = 0.3) +
  labs(
    title = "The best coffee comes from Papua New Guinea",
    subtitle = "Standardized total rating points of coffee around the world <br>Color coded for <span style='color:goldenrod3'>below</span></b> and <span style='color:#76EEC6'>above</span></b> the mean of zero <br>A higher score is better \n "   
  ) +
  theme(legend.position = "none",
        axis.text.x  = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        plot.background = element_rect(fill = "#362d26"),
        panel.background = element_rect(fill = "#362d26"), 
        panel.grid.major.y = element_blank(),
        title = element_text(size = 13),
        plot.subtitle = element_markdown(),
  )


ggsave("coffee_ratings.tiff", width = 235, height = 165, dpi = 400, units = "mm")

