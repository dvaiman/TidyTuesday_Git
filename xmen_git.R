# TidyTuesdat; depicted X-men


# packages
library(tidytuesdayR)
library(gganimate)
library(tidyverse)
library(ggtext)
library(ggdark)

# load datasets
tt_output <- tt_load("2020-06-30")

character_visualization <- tt_output$character_visualization
locations <- tt_output$locations
characters <- tt_output$characters
xmen_bechdel <- tt_output$xmen_bechdel

# Cumaulative % of depicted characters

a <- character_visualization %>%  separate(character, sep = " = ", into = c("costume", "non-costume")) %>%  
  group_by(costume, issue) %>% 
  summarise( across(thought:depicted | speech, sum)) %>% 
  mutate(sex=ifelse(costume == "Angel"| 
                      costume ==   "Ariel/Sprite/Shadowcat"| 
                      costume ==   "Banshee"|
                      costume ==   "Storm"|
                      costume ==   "Binary/Ms Marvel"|
                      costume ==   "Dazzler"|
                      costume ==   "Marvel Girl/Phoenix"|
                      costume ==   "Jubilee"|
                      costume ==   "Moira MacTaggert (scientist helper)*"|
                      costume ==   "Mystique"|
                      costume ==   "Phoenix(2)"|
                      costume ==   "Rougue"|
                      costume ==   "Psylocke",
                    "women", "men")) %>% 
  filter(!c(costume=="Editor narration" | costume=="Omnipresent narration")) %>% 
  ungroup() %>% 
  mutate(issue = as.numeric(issue)) %>% 
  arrange(issue)


# build rank, labels and relative values
p <- a %>% group_by(costume) %>% 
  mutate(depictedcum = cumsum(depicted)) %>% 
  ungroup() %>% 
  group_by(issue) %>% 
  mutate(rank = min_rank(-depictedcum) * 1) %>% 
  ungroup() %>% 
  filter(!issue>281) %>% 
  # plot
  ggplot(aes(-rank,depictedcum, fill = sex)) +
  geom_col(width = 0.8, position="identity") +
  coord_flip(clip = 'off') + 
  geom_text(aes(y = 0, label = paste(costume, " ")), vjust = 0.2, hjust = 1)+      # label
  geom_text(aes(-rank,y=depictedcum,label = depictedcum, hjust=0)) + # label
  dark_theme_void() +
  scale_fill_manual(values=c("#fc4a26", "#56B4E9")) +
  labs(title = " <strong><i><span style='color:yellow'>X-Men:</span></i></strong></b> ", 
       subtitle="Issue* {closest_state} - Cumulative % of depicted <strong><span style='color:#fc4a26'>Male</span></strong></b> and <strong><span style='color:#56B4E9'>Female</span></strong></b> characters", 
       x = "", y = "",
       caption = "*Timespan: 1963-1992") +
  theme(legend.position = "none",
        plot.title = element_markdown(size = 24, hjust = 0, vjust = 0.5, margin = ggplot2::margin(5, 0, 5, 0)),
        plot.subtitle = element_markdown(size = 18, hjust = 0, vjust = 0.5),
        axis.title = element_text(hjust = 0, size = 22, face = "bold", colour = "yellow"),
        axis.ticks.y = element_blank(),  # These relate to the axes post-flip
        axis.text.y  = element_blank(),  # These relate to the axes post-flip
        axis.ticks.x = element_blank(),  # These relate to the axes post-flip
        axis.text.x  = element_blank(),  # These relate to the axes post-flip
        plot.margin = margin(1,1,1,4, "cm")) +  
  # animate along Issue
  transition_states(issue,4,1) +
  view_follow()

# animate
animate(p, 100, fps = 10, duration = 45, width = 800, height = 600)

# save last made animation
anim_save("x_men.gif_v")
