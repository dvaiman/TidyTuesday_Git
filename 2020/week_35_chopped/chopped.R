
# TidyTuesday
# week 35 
# chopped
# Daniel Vaisanen
# knife borrowed from Georgious Karamanis
# @geokaramanis: https://github.com/gkaramanis/tidytuesday/blob/master/2020-week35/chopped.R



install.packages("geofacet")
library(tidyverse)
library(ggforce)
library(sf)
library(here)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')


chopped %>% count(judge1, sort = T)
chopped %>% count(judge2, sort = T)
chopped %>% count(judge3, sort = T)
chopped %>% count(judge1, judge2, judge3, sort = T)

chopped %>% count(entree, sort = T)
chopped %>% count(episode_rating, sort = T)


chopped %>% 
ggplot(aes(season ,episode_rating)) +
  geom_point(aes(size=episode_rating), alpha = 0.5) +
  theme_minimal()


?str_sub

# Bit of unnecessary coding

longchop <- chopped %>% 
  mutate(state1 = str_extract(contestant1_info , "[A-Z]{2}"),
         state2 = str_extract(contestant2_info , "[A-Z]{2}"),
         state3 = str_extract(contestant3_info , "[A-Z]{2}")) %>%  
  pivot_longer(-c(season:contestant4_info)) %>% 
  mutate(value =fct_lump_min(value, 4)) %>% drop_na(value) %>% 
  filter(!value=="Other") %>% 
  rename("state" = value) %>% 
  mutate(state1= case_when(state == "NY" ~"New York" , 
                           state =="NJ"~ "New Jersey",
                           state =="CA" ~ "California",
                           state =="CT" ~ "Connecticut",
                           state =="MA" ~ "Massachusets", 
                           state =="PA" ~ "Pensylvenia",
                           state =="FL" ~ "Florida",
                           state =="GA" ~ "Georgia",
                           state =="IL" ~ "Illinois",
                           state =="MD" ~ "Maryland",
                           state =="TN" ~ "Tennessee",
                           state =="LA" ~ "Louisiana",
                           state =="DC" ~ "District of Columbia",
                           state =="ME" ~ "Maine",
                           state =="AZ" ~ "Arizona",
                           state =="MS" ~ "Mississippi ",
                           state =="VA" ~ "Virginia ",
                           state =="NV" ~ "Nevada",
                           state =="CO" ~ "Colorado",
                           state =="TX" ~ "Texas",
                           state =="NC" ~ "North Carolina",
                           state =="OR" ~ "Oregon",
                           state =="SC" ~ "South Carolina",
                           state =="WA" ~ "Washington",
                           state =="OH" ~ "Ohio",
                           state =="MI" ~ "Michigan ",
                           state =="NM" ~ "Minnesota ",
                           state =="KY" ~ "Kentucky ")) %>% 
  mutate(episode_rating_diff = episode_rating - 8.4) %>% group_by(state1) %>% 
  mutate(state1 = fct_reorder(state1, episode_rating)) 

# data used in map
shortchop <- longchop %>% group_by(state) %>% summarise(count = n()) %>% 
  mutate(count = count)

# map

# map data:
# https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html

# read map data
usa <- st_read("2020/week_35_chopped/cb_2018_us_state_20m.shp")

# filter states
usa_28 <- usa %>%
    filter((STUSPS %in% c("NY", 
                          "NJ",
                          "CA",
                          "CT",
                          "MA",
                          "PA",
                          "FL",
                          "GA",
                          "IL",
                          "MD",
                          "TN",
                          "LA",
                          "DC",
                          "ME",
                          "AZ",
                          "MS",
                          "VA",
                          "NV",
                          "CO",
                          "TX",
                          "NC",
                          "OR",
                          "SC",
                          "WA",
                          "OH",
                          "MI",
                          "NM",
                          "KY")))

# join data frames
shopusa <- shortchop %>%
  left_join(usa_28, by = c("state" = "STUSPS"))


# create sf object 
shopusa <- st_sf(shopusa)

# dataset for handle
handle <- data.frame(
  x = c(-180, -120, -120, -180),
  y = c(43, 43, 52, 52))

# plot

ggplot(data = shopusa) +
  geom_text(aes(x = -72 ,y =20 ,label ="PED"), 
            size = 45, 
            angle = 340,
            color = "#f28500",
            family = "Luckiest Guy") +
  geom_shape(data = handle, aes(x = x, y = y ), 
             radius = unit(0.5, 'cm'),
             fill = "#3f2a14") +
  geom_rect(mapping=aes(xmin=-130, xmax=-60, ymin=25, ymax=52.1), fill = "#E0E0E0") +
  geom_rect(mapping=aes(xmin=-130, xmax=-60, ymin=25, ymax=23), fill = "lightgrey") +
  geom_sf(aes(fill= count), 
          color = "transparent") +
  geom_text(aes(x = -110 ,y =24.5 ,label ="CHOP"), 
            size = 45,
            color = "darkorange",
            family = "Luckiest Guy") +
  geom_text(aes(x=-61, y=26.1, label = "Source: Kaggle & IMDB | Grapics: Daniel Vaisanen"), 
            angle = 90,
            size = 2.8,
            hjust = 0,
            color = "grey50") +
  annotate("point", x = -135, y =47, size = 9, color = "#E0E0E0") +
  annotate("point", x = -145, y =47, size = 9, color = "#E0E0E0") +
  annotate("point", x = -155, y =47, size = 9, color = "#E0E0E0") +
  scale_fill_gradient2(
    low = "#E0E0E0", 
    mid = "#E0E0E0",
    high = "darkred", 
    trans = "log10",
    name = "Amount of contestants") +
  coord_sf(clip = "off") +
  theme_void() +
  theme(plot.background = element_rect(fill="#eaf1f8", color="#eaf1f8"),
        legend.position = c(0.265, 0.585),
        legend.direction = "horizontal",
        legend.background = element_rect(fill="#eaf1f8", color = "#eaf1f8"),
        legend.text = element_text(vjust = 1, family = "Luckiest Guy", size = 8),
        legend.title = element_text(vjust = 1, family = "Luckiest Guy" ), 
        legend.key.size = unit(0.9, "cm"),
        plot.title = element_text(size = 35, family = "Luckiest Guy", hjust = 0, color = "darkred"),
        plot.subtitle = element_text(size = 16, family = "Luckiest Guy", hjust = 0, color = "grey10"),
        plot.margin = margin(0.8, 0.2, 2, 0.2, "cm")) +
  labs(title = "     Something is missing...", 
       subtitle = "            USA according to the TV-serie Chopped: Map of contestants home state")

# save
ggsave(here("2020/week_35_chopped/", "chopped.png"),width = 32, height = 15.4, units = "cm", dpi = 300)



  
  
  
  
  