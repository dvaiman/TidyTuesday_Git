# author: Daniel Vöäisänen
# TidyTuesday: week 29 - Astronouts 

# Libraries
library(tidyverse)
library(ggalluvial)
library(ggimage)

# Data
astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')
image_background2 <- "https://c4.wallpaperflare.com/wallpaper/119/763/640/space-black-background-wallpaper-preview.jpg"

# Datamongin
astronauts1 <- astronauts %>% 
  select(id, 
         number, 
         occupation, 
         sex, 
         military_civilian, 
         nationality, 
         mission_number, 
         total_number_of_missions) %>%
  filter(mission_number==1) %>% 
  mutate(occupation=toupper(occupation)) %>% 
  mutate(nationality=fct_lump_min(nationality, 21), 
         occupation=fct_lump_min(occupation, 9)) %>% 
  group_by(nationality) %>% 
  mutate(nationality_n= n()) %>%
  ungroup %>%
  group_by(id) %>% 
  mutate(sex = str_to_title(sex),
         military_civilian = str_to_title(military_civilian),
         occupation = str_to_sentence(occupation)) %>% 
  mutate(occupation = case_when(occupation == "Psp" ~ "PSP",
                                occupation == "Msp" ~ "MSP",
                                TRUE ~ occupation)) %>% 
  filter(!occupation=="Other") %>% 
  group_by(id) %>% 
  mutate(count_id=n()) %>% 
  ungroup()

# Plot
p <- astronauts1%>% 
  ggplot(aes(y = count_id, 
             axis1 = occupation, 
             axis2 = nationality, 
             axis3 = sex, 
             axis4 = military_civilian)
  ) +
  geom_alluvium(aes(fill = occupation), 
                aes.bind=TRUE, 
                width = .7, 
                alpha=0.5) +
  geom_stratum( width = 1/100,   
                fill = c("black"),
                color = c("#EFCC98",
                          "#F98477",
                          "#ED5983", 
                          "#A3319F", 
                          "#782B9D", 
                          "#F98477", 
                          "#4B2991",  
                          "#A3319F", 
                          "#EFCC98", 
                          "#F98477", 
                          "#ED5983",
                          "#EFCC98")
  )+
  geom_text(stat = "stratum", 
            aes(label = after_stat(stratum)), 
            alpha = 0.7, 
            nudge_x = 0.02, 
            hjust = 0, 
            nudge_y = 0, 
            color = "grey70",
            family = "Orbitron",
            size = 3.5) +
  scale_x_discrete(limits = c("Occupation", 
                              "Nationality", 
                              "Sex", 
                              "Military VS Civilians"),
                   expand = c(.05, .05)) +
  scale_fill_viridis_d(option = "magma", begin = 0.4) +
  theme_void() +
  labs(
    title = "OCCUPATIONS \nIN SPACE\n\n\n\n",
    subtitle = "Distribbution of space occupation charasteristica",
    caption = "Daniel Väisänen | Data: Mariya Stavnichuk & Tatsuya Corlett"
  ) +
  theme(text=element_text(size=12,  
                          family="Orbitron"),
        legend.position = "none",
        plot.background = element_rect(fill = "black"),
        plot.margin = unit(c(2,2,2,2), "lines"),
        plot.title = element_text(colour = "white", 
                                  size = 24, 
                                  margin = margin(t = 10), 
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(colour = "grey50", 
                                     margin = margin(t = 1), 
                                     hjust= 0.5),
        plot.caption = element_text(colour = "grey30", 
                                    margin = margin(5,5,5,5),
                                    hjust = 0.5,
                                    size = 8)) +
  coord_cartesian(clip = "off")

ggbackground(p, background = image_background2)


ggsave("occupations_in_space.png", device = "png", dpi = 300, width = 210, height = 130, units = "mm")

