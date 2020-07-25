
# TidyTuesday - week 30
# australian animals

#libraries
library(tidyverse)
library(ozmaps)
library(sf)
library(patchwork)
library(ggrepel)
library(ggtext)
library(here)



# read data
animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

# read map data
oz_states <- ozmaps::ozmap_states

# long format and new names
animal_outcomes2 <- animal_outcomes %>% 
  pivot_longer(-c(year, animal_type, outcome, Total),
               names_to = "states",
               values_to = "amount") %>% 
  mutate(states2 = case_when(states == "ACT" ~ "Australian Capital Territory",
                             states == "NSW" ~ "New South Wales",
                             states == "NT" ~ "Northern Territory", 
                             states == "QLD" ~ "Queensland",
                             states == "SA" ~ "South Australia",
                             states == "TAS" ~ "Tasmania",
                             states == "VIC" ~ "Victoria",
                             states == "WA" ~ "Western Australia"))   %>% 
  mutate(population = case_when(states == "ACT" ~ 366900,
                                states == "NSW" ~ 7317500,
                                states == "NT" ~ 231200, 
                                states == "QLD" ~ 4599400,
                                states == "SA" ~ 1659800,
                                states == "TAS" ~ 511000,
                                states == "VIC" ~ 5640900,
                                states == "WA" ~ 2366900)) 



a<- animal_outcomes2 %>% 
  filter(outcome == "Euthanized" | 
           outcome == "Reclaimed" |
           outcome == "Rehomed" ) %>% 
  filter(
    animal_type == "Livestock" |
      animal_type == "Horses" |
      animal_type == "Dogs" |
      animal_type == "Cats") %>% 
  group_by(states2, outcome, population) %>% 
  summarise(amount=sum(amount, na.rm = T)) %>% 
  ungroup() %>% 
  select(amount, states2, outcome, population) %>% 
  left_join(oz_states, by = c("states2" = "NAME")) 



# longitude and latitude 
df_labs <-
  tibble(
    states2 = unique(animal_outcomes2$states2),
    x = c(148, 147, 133.5, 144, 135, 148, 144, 121.5),
    y = c(-35, -32, -20, -23, -29, -43, -37, -26)
  ) 
# merge dataframe with longitude and latitude
a <- a %>%  
  left_join(df_labs)


# make geom sf object
a<-st_as_sf(a) 

# plots
# facet map plots
p1 <- ggplot(a) + 
  geom_sf(mapping = aes(fill = amount), fill = "lightblue", show.legend = FALSE , alpha=1, color="#8DB6CD") + 
  coord_sf() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_point(mapping = aes(x, y, size = amount), 
             color = "#8B0A50",
             show.legend = FALSE ) +
  facet_wrap(~outcome) +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    plot.background = element_rect(fill="#8DB6CD", color="#8DB6CD")
  )


# map plot with population data
p2 <- ggplot(a) + 
  geom_sf(mapping = aes(fill = population),  
          show.legend = FALSE, 
          alpha=1, 
          color="#8DB6CD") + 
  coord_sf() +
  scale_fill_gradient(low = "lightblue", 
                      high = "darkblue") +
  geom_text(x = 135, 
            y = -9, 
            label = "Darker blue = larger population",
            color = "grey30",
            family = "Merienda",
            size = 2.8) +
  geom_text_repel(df_labs, mapping= aes(x=x, 
                                        y=y, 
                                        label = states2),   
                  size = 2.8,
                  family = "Merienda",
                  lineheight = .9,
                  nudge_x = 0,
                  nudge_y = 1,
                  color = "#8B0A50") +
  xlim(110,
       165) +
  ylim(-45, 
       -8) +
  theme_void() +
  theme(
    plot.margin = margin(0, 
                         0.2, 
                         0, 
                         0.2, 
                         "cm"),
    plot.background = element_rect(fill = "#8DB6CD", 
                                   color = "#8DB6CD")
  )


p3 <- animal_outcomes2 %>% 
  filter(outcome == "Euthanized" | 
           outcome == "Reclaimed" | 
           outcome == "Rehomed" ) %>% 
  filter(
    animal_type == "Livestock" |
      animal_type == "Horses" |
      animal_type == "Dogs" |
      animal_type == "Cats") %>% 
  group_by(animal_type, 
           outcome) %>% 
  summarise(Total=sum(Total)) %>%
  group_by(outcome) %>% 
  mutate(percentage=(Total/sum(Total)*100)) %>% 
  mutate(percentage = round(percentage, 1)) %>% 
  mutate(animal_type = fct_reorder(animal_type, Total)) %>% 
  ggplot() +
  geom_segment(aes(percentage, 
                   animal_type,  
                   yend = animal_type), 
               xend = 0.1, 
               color="grey40",
               linetype= 3,
               size = 1)+
  geom_vline(xintercept=0, 
             color="grey") +
  geom_point(aes(percentage, 
                 animal_type, 
                 color=percentage), 
             alpha = 1,
             size = 9)+
  geom_text(aes(percentage, 
                animal_type, 
                label = percentage), 
            color = "white", 
            size = 2.5)+
  scale_color_gradient(low = "lightblue", 
                       high = "#8B0A50") +
  facet_wrap(~outcome) +
  labs(x = "Percentage (%)",
       caption = "\nVisualization: Daniel Väisänen | TidyTuesday") +
  theme_minimal() +
  theme(
    legend.position = 'none',
    strip.text = element_blank(),
    axis.title.x=element_text(color = "grey50", hjust = 0.9),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_text(),
    axis.ticks.y=element_blank(),
    axis.line.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(0, 0.2, 0.2, 0.4, "cm"),
    plot.background = element_rect(fill="#8DB6CD", color = "#8DB6CD"))


  # join plots
  
  patch<-p1/p3 +
  plot_layout(widths = c(16, 16), 
              heights = unit(c(7 , 6), 
                             c('cm', 'null')))


p2/patch +
  plot_layout(widths = c(15, 15),
              heights = unit(c( 10 , 5), 
                             c('cm', 'null'))) +
  plot_annotation(title = '\nAustralia -Euthanized, Reclaimed and Rehomed animals',
                  subtitle = "Between 1999-2018 -905 242 cats, dogs, livestock and horses were euthanized, 
410 094 were reclaimed and 822 402 were rehomed. Most of these were in the
highly populated states of Victoria, New South Wales and Queensland\n\n",
                  theme = theme(plot.title = element_text(size = 18))) & 
  theme(text = element_text('Merienda')) & 
  theme(plot.background = element_rect(fill = "#8DB6CD"))


ggsave(here("2020", "week_30", "australia_animals.png"), width = 10, height =11, dpi = 300)


# get count of different animals
animal_outcomes2 %>% 
  filter(outcome == "Euthanized" | 
           outcome == "Reclaimed" | 
           outcome == "Rehomed" ) %>% 
  filter(
    animal_type == "Livestock" |
      animal_type == "Horses" |
      animal_type == "Dogs" |
      animal_type == "Cats") %>% 
  group_by(outcome, Total) %>% 
  summarise(n()) %>% 
  group_by(outcome) %>% 
  summarise(sum(Total))


