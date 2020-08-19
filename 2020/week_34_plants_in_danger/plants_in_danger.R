

# week 34 2020- Plants in Danger
# Daniel VÄisänen


install.packages("tibble")
library(tidyverse)
library(ggforce)
library(patchwork)
library(ggtext)
library(broom)
library(here)

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')








a <- threats %>% count(year_last_seen, continent, red_list_category) %>% 
  drop_na(year_last_seen,red_list_category,continent) %>% 
  filter(!year_last_seen == "Before 1900") %>% 
  mutate(red_list_num=case_when(red_list_category == "Extinct" ~ 1,
                   red_list_category == "Extinct in the Wild" ~ 0)) %>% 
  group_by(red_list_category) %>% 
  mutate(percentage=((n*red_list_num)/sum(n)*100)) %>% 
  mutate(year_last_seen = fct_rev(year_last_seen)) %>% 
  mutate(continent = fct_reorder(continent, percentage, .desc = TRUE))



 p1 <- ggplot(a, aes(fill = continent, alpha = n)) +
  geom_ellipse(aes(x0 = 7, y0 = 0, a = 6, b = 3, angle = 0, m1 = 2),  color = "transparent") + #right
  geom_ellipse(aes(x0 = -7, y0 = 0, a = 6, b = 3, angle = pi, m1 = 2),  color = "transparent") + #left
  geom_ellipse(aes(x0 = 0, y0 = 7, a = 6, b = 3, angle = pi / 2, m1 = 2), color = "transparent") + #top
  geom_ellipse(aes(x0 = 5, y0 = 5, a = 6, b = 3, angle = pi / 4, m1 = 2), color = "transparent") + #dia_top_right
  geom_ellipse(aes(x0 = -5, y0 = 5, a = 6, b = 3, angle = 3*pi / 4, m1 = 2),  color = "transparent") + #dia_top_left
  geom_ellipse(aes(x0 = 5, y0 = -5, a = -6, b = 3, angle = 7*pi / 4, m1 = 2),  color = "transparent") + #dia_bottom_right
  geom_ellipse(aes(x0 = -5, y0 = -5, a = -6, b = 3, angle = 5*pi / 4, m1 = 2), color = "transparent") + #dia_bottom_left
  geom_ellipse(aes(x0 = 0, y0 = -7, a = 6, b = 3, angle = 3*pi / 2, m1 = 2),  color = "transparent") + #bottom
  geom_circle(aes(x0 = 0, y0 = 0, r= log(percentage*10)), fill = "orange", alpha = 1, color = "darkorange") +
  coord_fixed(xlim = c(-15, 15)) +
  facet_grid(year_last_seen ~ continent, switch = "y") +
  theme_void() +
  theme(
    plot.background = element_rect(fill="ivory", color = "ivory"),
    strip.text.y.left = element_text(angle = 0, size = 12),
    strip.text.x = element_text( size = 12),
    legend.position = "none",
    plot.margin = margin(2, 0, 2, 2)
    )
  

p2 <- ggplot() +
  geom_ellipse(aes(x0 = 5, y0 = -17, a = 6, b = 1.5, angle = pi / 4, m1 = 2), fill="#919c4c", alpha = .7, color = "transparent") + #dia_top_right
  geom_ellipse(aes(x0 = -4.5, y0 = -17, a = 6, b = 1.5, angle = 3*pi / 4, m1 = 2), fill="#919c4c", alpha = .7, color = "transparent") +
  geom_bspline_closed(aes(x = c(0, -2, 2, 0), y = c(0,-10, -50, 5) ), color="#919c4c", alpha = .9) +
  geom_ellipse(aes(x0 = 7, y0 = 0, a = 6, b = 3, angle = 0, m1 = 2), fill= "#f5c04a", alpha = 0.5, color = "transparent") + #right
  geom_ellipse(aes(x0 = -7, y0 = 0, a = 6, b = 3, angle = pi, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #left
  geom_ellipse(aes(x0 = 0, y0 = 7, a = 6, b = 3, angle = pi / 2, m1 = 2), fill= "#f5c04a", alpha = 0.5, color = "transparent") + #top
  geom_ellipse(aes(x0 = 5, y0 = 5, a = 6, b = 3, angle = pi / 4, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #dia_top_right
  geom_ellipse(aes(x0 = -5, y0 = 5, a = 6, b = 3, angle = 3*pi / 4, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #dia_top_left
  geom_ellipse(aes(x0 = 5, y0 = -5, a = -6, b = 3, angle = 7*pi / 4, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #dia_bottom_right
  geom_ellipse(aes(x0 = -5, y0 = -5, a = -6, b = 3, angle = 5*pi / 4, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #dia_bottom_left
  geom_ellipse(aes(x0 = 0, y0 = -7, a = 6, b = 3, angle = 3*pi / 2, m1 = 2), fill="#f5c04a", alpha = 0.5, color = "transparent") + #bottom
  geom_circle(aes(x0 = 0, y0 = 0, r= 3), fill = "orange", alpha = 1, color = "transparent") +
  geom_circle(aes(x0 = 0, y0 = 0, r= 2.8), fill = "white", alpha = 0.2, color = "transparent") +
  geom_arc(aes(x0 = 0, y0 = -22, r = 6, start = -1.3, end = -5), color="blue", alpha = .0) +
  theme_void() +
  coord_fixed(xlim = c(-25, 25), ylim = c(-70,15), clip = "off") +
  geom_textbox(aes(x= -25, y = 14, label = "**Size**: Log scaled percentages of\n extinct species"),
               size = 4,
               fill = NA,
               box.colour = NA,
               hjust = 0,
               family = "Merienda") +
  geom_textbox(aes(x= 2, y = -25, label = "**Transparance**: amount of\n plants extinct in the wild"),
               size = 4,
               fill = NA,
               box.colour = NA,
               hjust = 0,
               family = "Merienda")+
  geom_textbox(aes(x= -25, y = -50, label = "Major threats to biodiversity, especially in areas of exceptional plant diversity, 
                   primarily in the tropics, are often linked to industrial-scale activities such as timber exploitation or large plantations, 
                   mining, and agriculture. Continents are sorted on percentage of most extinct plant species. Africa has had the most extinct plants while Europe has the least.
                   The number one threat to plants is pollution and . Still in 2000-2020 plant species are going extinct."), 
               width = unit(5, "inch"),
               color = "black",
               lineheight = 1.7,
               size = 4,
               fill = NA,
               box.colour = NA,
               hjust = 0,
               family = "Merienda") +
  geom_curve(aes(x = 0, y = 0, xend = -20, yend = 10),
             arrow = arrow(length = unit(0.07, "inch")), 
             size = 0.4,
             color = "gray50", 
             curvature = -0.3) +
geom_curve(aes(x = 8, y = 8, xend = 20, yend = -20),
           arrow = arrow(length = unit(0.07, "inch")), 
           size = 0.4,
           color = "gray50", 
           curvature = -0.5) +
  theme(plot.background = element_rect(fill="ivory", color = "ivory"),
        plot.margin = margin(2, 2, 2, 0))


p1 + p2  + 
  plot_annotation(title = 'Extinct and Endangered Plants\n',
                  caption = "Source | Graphics: Daniel Vaisanen",
                  theme = theme(plot.title = element_text(size = 45, 
                                                          hjust = 0.5, 
                                                          family = "Playfair Display",
                                                          face = "bold"))) & 
  theme(text = element_text('Merienda'),
        plot.background = element_rect(fill="ivory", color = "ivory"),
        plot.margin = margin(20, 20, 20, 20))

ggsave(here("2020/week_34_plants_in_danger/plants.png"), dpi=300, width = 18, height = 12)



  library(tidymodels)
  
  # clean dataset for logistic regression
 plants2 <- plants %>% 
   mutate(red_list_num=case_when(red_list_category == "Extinct" ~ 1,
     red_list_category == "Extinct in the Wild" ~ 0)) %>% 
   select(threat_AA:threat_GE, red_list_num)  
 
 plants3 <- bootstraps(plants2, times = 5, apparent = F) %>% unnest(cols = c(splits))
 
 
 
 
           
           # modelfit
fit <- glm(formula = red_list_num ~ ., family = binomial(link = "logit"), 
      data = plants2) 

tidied <- tidy(fit, conf.int =T, exponentiate = T) %>% slice(-1)

# ==================================

p3 <- tidied %>% 
  mutate(term = case_when(term== "threat_AA" ~ "Agriculture & Aquaculture",
                   term== "threat_BRU"~"Biological Resource Use", 
                   term== "threat_RCD"~"Commercial Development",  
                   term== "threat_ISGD" ~"Invasive Species",      
                   term== "threat_EPM" ~"Energy Production & Mining",
                   term== "threat_CC" ~"Climate Change",          
                   term== "threat_HID" ~"Human Intrusions",        
                   term== "threat_P" ~"Pollution",      
                   term== "threat_TS" ~"Transportation Corridor", 
                   term== "threat_NSM" ~"Natural System Modifications",
                   term== "threat_GE" ~    "Geological Events" )) %>% 
  mutate(term = fct_reorder(term, estimate)) %>% 
  ggplot() +
  geom_linerange(aes(y = term, xmin = conf.low, xmax = conf.high), color= "orange", size = 5, alpha = 0.5) +
  geom_point(aes(x=estimate, y=term),size=3, color = "blue") + 
  geom_vline(aes(xintercept=1)) +
  scale_x_log10()+
  labs(title = "Of 11 treaths for plants only three were significantly different from 1 in a logistic regression model") +
  theme_minimal() +
  theme(plot.background = element_rect(fill="ivory", color = "ivory"))



plants2 %>%
  nest(plants2 = -c(continent, year_last_seen)) %>% 
  mutate(
    fit2 = map(plants2, ~ glm(formula = red_list_num ~ ., 
                              family = "binomial", 
                              data = .x),
    tidied = map(fit2, tidy,)
  ) %>% 
  unnest(tidied)

  
  conf.int =T,
  exponentiate = T),

  
  
  
 tidied %>% mutate(term = fct_reorder(term, estimate, .desc = TRUE)) %>% 
 ggplot(aes(term, estimate, group = 1)) + 
    geom_line(aes(color = term)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = type), alpha = .2) + 
    theme_minimal() 

 
 nested <-   plants %>% mutate(red_list_num=case_when(red_list_category == "Extinct" ~ 1,
                                                                 red_list_category == "Extinct in the Wild" ~ 0)) %>% 
   select(threat_AA:threat_GE, red_list_num, year_last_seen) %>% group_by(year_last_seen)  %>% nest()
 
 # Nest model and add to datafram | augment not working yet
 
glm_mod <-nested %>% mutate(
  fit2 = map(data, ~ glm(red_list_num ~ ., 
      data = .x)),
   tidied = map(fit, tidy))
  
cox_models <-nested %>% mutate(
  fit = map(data, ~ coxph(Surv(fupyrs_chd, chd_event) ~ PhysicalWorkSituation_num + Age + Gender, data = .x)),
  tidied = map(fit, tidy),
  glanced = map(fit, glance),
  augmented = map(fit, augment)
)




  plot_layout( widths = unit(c(10, 5), c('cm', 'null')))


  (p1+p2)/p3  + 
  plot_layout( heights = unit(c(3, 3, 1), c('cm', 'null')))
  

# Data wrangling (I love forcats!)
threats_per_cont <- 
  threats %>% 
  filter(threatened != 0) %>% 
  group_by(continent) %>% 
  count(threat_type, name = "n_threats") %>% 
  mutate(
    threat_type = str_replace_all(threat_type, " ", "\n"),
    continent = str_replace(continent, " ", "\n"),
    total_extinctions_per_cont = sum(n_threats)
  ) %>% 
  ungroup() %>% 
  group_by(threat_type) %>% 
  mutate(
    total_extinctions_per_threat = sum(n_threats)
  ) %>% 
  ungroup() %>% 
  mutate(
    continent = fct_reorder(continent, total_extinctions_per_cont),
    threat_type = fct_reorder(threat_type, -total_extinctions_per_threat),
    threat_type = fct_relevel(threat_type, "Unknown", after = Inf)
  ) %>% 
  group_by(threat_type) %>% 
  summarise(n_threats=sum(n_threats)) %>% 
  summarise(threat_type=threat_type, threat_percent = n_threats/sum(n_threats)) %>% 
    add_column(y= 1, x = 1:12) 


filter(!threat_type==unknown)


library(gggibbous)


p3 <- ggplot(threats_per_cont, aes(x, y)) +
geom_moon(aes(ratio = threat_percent), size = 20, fill = "black") +
  geom_text(aes(y = 0.9 , label = threat_type)) +
  ylim(0.8,1.2)+
  theme_void() +
  theme(plot.background = element_rect(fill="ivory", color = "ivory"))



data.frame(x = 1:12, y = 0), aes(x = x, y = y))
