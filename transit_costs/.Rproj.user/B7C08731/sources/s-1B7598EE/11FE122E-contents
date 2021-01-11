
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# road accidents in Victoria
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggtext)

# load data ---------------------------------------------------------------

vic_crash_data <- read_csv("Crashes_Last_Five_Years.csv")

# data manipulation -------------------------------------------------------

vic_crash_data <- vic_crash_data %>% 
  janitor::clean_names() %>% janitor::remove_empty() %>% 
  mutate(accident_date = dmy(accident_date),
         accident_time = hms(accident_time))

# generate bounding box for metro melbourne
metro_box <- vic_crash_data %>% 
  filter(stat_div_name == "Metro") %>% 
  summarise(
    max_long = max(longitude, na.rm = TRUE),
    min_long = min(longitude, na.rm = TRUE),
    max_lat = max(latitude, na.rm = TRUE),
    min_lat = min(latitude, na.rm = TRUE)
  )

# generate bounding box for zoomed map
zoomed_box <- vic_crash_data %>% 
  summarise(
    max_long = max(longitude, na.rm = TRUE) + 1.5,
    min_long = max(longitude, na.rm = TRUE) - 4.25,
    max_lat = max(latitude, na.rm = TRUE) + 1,
    min_lat = max(latitude, na.rm = TRUE) - 1.5
  )

# plot --------------------------------------------------------------------

## metro plot
vic_crash_data %>% 
  filter(stat_div_name == "Metro") %>% 
  mutate(major_crash = case_when(
    fatality > 0 ~ "major",
    seriousinjury > 0 ~ "major",
    TRUE ~ "minor"
  )) %>% 
  ggplot(aes(longitude, latitude, color = major_crash)) +
  geom_point(alpha = 0.3, size = 0.3, show.legend = FALSE) +
  labs(y = NULL, x = NULL, color = NULL) +
  scale_color_manual(values = c("orange", "gray30")) +
  scale_alpha_discrete(range = c(0.7, 0.2)) +
  scale_x_continuous(limits = c(metro_box$min_long, metro_box$max_long), 
                     expand = c(0,0)) + 
  scale_y_continuous(limits = c(metro_box$min_lat, metro_box$max_lat), 
                     expand = c(0,0)) + 
  theme_bw() + 
  theme(legend.position = "none",
        panel.border = element_rect(color = "transparent"),
        text = element_text(family = "Verdana", color = "#A8A7A7"),
        plot.title = element_text(family = "Verdana", face = "bold", size = 14),
        plot.subtitle = element_markdown(lineheight = 1.2),
        strip.text = element_text(size = 14, color = "#A8A7A7"),
        strip.background = element_rect(color = "#474747", fill = "#474747"),
        plot.background = element_rect(color = "#474747", fill = "#474747"),
        panel.background = element_rect(color = "transparent", fill = "#474747"),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) -> metro_plot


## full accidents plot
vic_crash_data %>% 
  mutate(major_crash = case_when(
    fatality > 0 ~ "major",
    seriousinjury > 0 ~ "major",
    TRUE ~ "minor"
  )) %>% 
  ggplot(aes(longitude, latitude, color = major_crash)) +
  geom_point(alpha = 0.3, size = 0.3, show.legend = FALSE) +

  # add bounding box
  geom_rect(data = metro_box, inherit.aes = FALSE,
            mapping = aes(
              xmin = min_long,
              xmax = max_long,
              ymin = min_lat,
              ymax = max_lat
            ),
            colour = "#363636", size = 1, fill = NA) +
  
  # add zoomed box
  geom_rect(data = zoomed_box, inherit.aes = FALSE,
            mapping = aes(
              xmin = min_long,
              xmax = max_long,
              ymin = min_lat,
              ymax = max_lat
            ),
            colour = "#363636", size = 1.5, fill = NA) +
  
  # add "zoom lines"
  annotate("segment",
           x = metro_box$min_long, xend = zoomed_box$min_long,
           y = metro_box$max_lat, yend = zoomed_box$max_lat,
           color = "#363636", size = 1, linetype = 2) +
  annotate("segment",
           x = metro_box$max_long, xend = zoomed_box$max_long,
           y = metro_box$min_lat, yend = zoomed_box$min_lat,
           color = "#363636", size = 1, linetype = 2) +
  
  # place zoomed metro plot in
  annotation_custom(
    ggplotGrob(metro_plot),
    xmin = zoomed_box$min_long,
    xmax = zoomed_box$max_long,
    ymin = zoomed_box$min_lat,
    ymax = zoomed_box$max_lat
  ) +
  
  # labeling
  labs(title = "Serious injury road accidents in Victoria",
       subtitle = "All road accidents involving injury or fatalty from 2012 to 2020",
       caption = "plot by Josh Faure ¦ Source: Victorian Department of Transport",
       y = NULL, x = NULL, color = NULL) +
  
  # adjust points colour and transparency
  scale_color_manual(values = c("orange", "gray40")) +
  scale_alpha_discrete(range = c(0.6, 0.3)) +
  
  # theming
  theme_bw() + 
  theme(legend.position = "none",
        panel.border = element_rect(color = "transparent"),
        text = element_text(family = "Verdana", color = "#A8A7A7"),
        plot.title = element_text(family = "Verdana", face = "bold", size = 14),
        plot.subtitle = element_markdown(lineheight = 1.2),
        strip.text = element_text(size = 14, color = "#A8A7A7"),
        strip.background = element_rect(color = "#474747", fill = "#474747"),
        plot.background = element_rect(color = "#363636", fill = "#363636"),
        panel.background = element_rect(color = "transparent", fill = "#474747"),
        panel.grid = element_blank(),
        axis.text = element_blank()) -> complete_plot


# export plot -------------------------------------------------------------
ggsave(complete_plot, filename = "plots/serious_road_accidents.png", height = 7, width = 7, units = "in")





