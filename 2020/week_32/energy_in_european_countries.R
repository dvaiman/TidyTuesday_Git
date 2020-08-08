
# TidyTuesday 
# week 32
# Energy production, import and export in European countries

library(tidyverse)
library(here)

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')


 # data for Total net production
ct <- country_totals %>% janitor::clean_names() %>% 
  mutate(country_name = replace_na(country_name, "United Kingdom")) %>% 
  filter(type=="Total net production")


# plot
country_totals %>% janitor::clean_names() %>% 
  mutate(country_name = replace_na(country_name, "United Kingdom")) %>% 
  ggplot() +
  geom_bar(aes(x=reorder(country_name,x2018),
               y=x2018/2),
           stat = "identity",width = 1,
           data=. %>% filter(type=="Total net production"), fill = "grey10") +
  geom_bar(aes(x=reorder(country_name,x2018),
               y=-x2018/2),
           stat = "identity",width = 1,
           data=. %>% filter(type=="Total net production"), fill = "grey10") +
  geom_bar(aes(x=reorder(country_name,x2018),
               y=-x2018),
           stat = "identity",width = 0.9,
           data=. %>% filter(type=="Imports"), fill = "#b3b300") +
  geom_bar(aes(x=reorder(country_name,x2018),
               y=x2018), 
           stat = "identity",width = 0.9,
           data=. %>% filter(type=="Exports"), fill = "#e6e600") +
  geom_text(aes(x= -4, 
                y= 160000, 
                label= "Exports"), 
            color = "#004b75",
            alpha = 0.2,
            size = 12,
            angle = 0,
            family = "IBM Plex Sans") +
  geom_text(aes(x= -4, 
                y= -160000, 
                label= "Imports"), 
            color = "#004b75",
            alpha = 0.2,
            size = 12, 
            angle = 0,
            family = "IBM Plex Sans") +
  geom_text(aes(x= 43, 
                y= -9000, 
                label= "Total production"), 
            color = "#004b75",
            alpha = 0.2,
            size = 17, 
            angle = 0,
            family = "IBM Plex Sans")+
  geom_text(data = ct, mapping=aes(
    x= country_name, 
    y= -x2018/2,
    label= country_name, 
  ), 
  size = 3,
  angle = 0,
  hjust=1,
  vjust=0.5,
  nudge_y = -6000,
  family = "Roboto") +
  geom_text(data = ct, mapping=aes(
    x= country_name, 
    y= x2018/2,
    label= country_name, 
  ), 
  size = 3,
  angle = 0,
  hjust=0,
  vjust=0.5,
  nudge_y = 6000,
  family = "Roboto") +
  
  #Totals
  geom_segment(aes(x = 40, y = 0, xend = 40, yend = 340000), 
               color = "grey10",
               lineend = "round",
               linejoin = "round",
               size = 1, arrow = arrow(length = unit(0.2, "inches"))) +
  #Totals
  geom_segment(aes(x = 40, y = 0, xend = 40, yend = -340000), 
               color = "grey10",
               lineend = "round",
               linejoin = "round",
               size = 1, arrow = arrow(length = unit(0.2, "inches"))) +
  #Exports
  geom_segment(aes(x = -1.5, y = 25000, xend = -1.5, yend = 140000), 
               size = 1, 
               color = "#e6e600",
               lineend = "round",
               linejoin = "round",
               arrow = arrow(length = unit(0.2, "inches"))) +
  #Imports
  geom_segment(aes(x = -1.5, y = -25000, xend = -1.5, yend = -140000), 
               size = 1, 
               color = "#b3b300",
               lineend = "round",
               linejoin = "round",
               arrow = arrow(length = unit(0.2, "inches"))) +
  labs(title = "\n      Energy in European countries\n\n",
       caption = "\n\n\nData: Eurostat | Graphics: Daniel Väisänen") +
  theme_void() +
  theme(plot.margin = unit(c(1,1,1,1), "cm"),
        plot.background = element_rect(fill="#00324e", color = "#00324e"),
        panel.background = element_rect(fill="#00324e", color = "#00324e"),
        plot.caption = element_text(angle = 0, vjust = 0.5, hjust = 0.5, family = "Roboto", size = 6)) +
  coord_flip(clip = "off")

ggsave(here("2020", "week_32", "energy_in_european_countries.png"), width = 6.5, height = 8, dpi=300)
