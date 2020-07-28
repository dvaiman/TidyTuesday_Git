# Author: Daniel Väisänen
# TidyTuesday week 31 -Penguins

library(tidyverse)
library(visdat)
library(cowplot)
library(ggrepel)
library(ggplot2) # For graphing
library(mapproj) # For the polar orthographic map projection
library(ggthemes) # For theme_map()
library(here)
library(extrafont)
# data
penguins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv')



# Get geospatial data for Antarctica only
antarctica <- map_data("world", region = "Antarctica")

# map of antartica
df_penguinloc <-
  tibble(
    island = c("Dream", "Biscoe", "Torgersen"),
    lat_y = c(-64.7333, -65.4333, -64.7666636),
    long_x = c(-64.2333, -65.5000, -64.083333)
  ) 


df_penguinloc <- penguins %>% 
  group_by(island) %>%  summarise(amount=n()) %>% 
  left_join(df_penguinloc, by = "island")


# "#B1BFCD", "#869BB1", "#68829E" greys
# barplot
d <- penguins %>% filter(year==2009) %>% 
  mutate(island = factor(island), 
         island = factor(island, levels = rev(levels(island)))) %>%  
  ggplot() +
  stat_count(aes(island, fill = species), alpha = 0.8) +
  annotate("text", y=3,  x= "Torgersen", label= "Torgersen", color = "#1874CD", family = "Oswald")+
  annotate("text", y=3,  x= "Dream", label= "Dream", color = "#c02728", family = "Oswald")+
  annotate("text", y=3,  x= "Biscoe", label= "Biscoe", color = "#53868B", family = "Oswald")+
  scale_fill_manual(values = c("#009ACD", "#869BB1", "#68829E")) +
  scale_y_reverse()+
  labs(caption = "Source: Dr. Kristen Gorman and the Palmer penguins | Graphics: Daniel Väisänen") +
  theme_minimal(base_family = "Oswald") +
  theme(legend.position = c(0.2,0.3),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill="ivory", color = "ivory"))


# main map
p <- ggplot(antarctica, aes(long, lat, group = group)) +
  geom_polygon(fill = "#506B8E", alpha = .8) +
  coord_map("ortho", orientation = c(-90, 0, 0),
            xlim = c(-62, -55),
            ylim = c(-75, - 60)) +
  geom_text_repel(df_penguinloc, mapping=aes(long_x, lat_y, label = island), 
                  group=1, color = c("#53868B", "#c02728", "#1874CD"), family = "Oswald",
                   box.padding = 0.5,
                   nudge_y = 1,  nudge_x = -2, min.segment.length = 0) +
  geom_point(df_penguinloc, mapping=aes(long_x, lat_y,  
                                        group = 1, 
                                        colour = island), 
             alpha =.7)+
  scale_color_manual(values = c("#53868B", "#c02728", "#1874CD"))+
  labs(title = "Penguins in Palmer Archipelago",
       subtitle = "Recorded penguins in 2009 and their nesting Islands") +
  theme_map(base_family = "Fredericka the Great") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.subtitle =  element_text(hjust = 0.5),
        plot.background = element_rect(fill="ivory", color = "ivory"))


# inset map
inset <- ggplot(antarctica, aes(long, lat, group = group)) +
  geom_polygon(fill = "#506B8E", alpha = .5) +
  coord_map("ortho", orientation = c(-90, 0, 0)) +
  geom_point(df_penguinloc, mapping=aes(long_x, lat_y,  
                                        group = island, 
                                        colour = island), 
             alpha =.5, size = 1)+
  annotate("rect", color="black", fill = "transparent",
           xmin = -68, xmax = -54,
           ymin = -75, ymax = -60)+
  labs(title = "Antarctica") +
  theme_map(base_family = "Fredericka the Great") +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(colour="grey"),
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill="ivory", color = "ivory"))



# draw together inset and main map
a <- ggdraw(p ) +
  draw_plot(inset, .47, .38, .5, .4)


# plot everything together
p1 <- plot_grid(a,d, ncol = 1, rel_widths = c(4, 2), rel_heights = c(2,1))+
  theme(plot.background = element_rect(fill="ivory")) +
  labs(title = "Penguins in Palmer Archipelago")

# import image
penguin <-here("2020/week_31/penguin.png")
                    
# draw plot with image
p2 <- ggdraw() + 
  draw_plot(p1) +
  draw_image(penguin, x = 0.2, y = 0.34, hjust = 1, width = 0.10, height = 0.15)

# save plot
ggsave(here("2020/week_31/penguinplot.png"))


