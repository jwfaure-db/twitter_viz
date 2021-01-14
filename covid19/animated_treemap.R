# code does not work right now due to incompatibility of treemapify and gganimate
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# Australian covid cases
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(coronavirus)
library(treemapify)
library(gganimate)


# load data ---------------------------------------------------------------

update_dataset()
data("coronavirus")


# data manipulation -------------------------------------------------------

aus_daily_data <- coronavirus %>% 
  filter(country == "Australia") %>% 
  group_by(province, type, date) %>%
  summarise(total_cases = sum(cases)) %>%
  pivot_wider(names_from = type, values_from = total_cases) %>%
  arrange(date) %>%
  mutate(active = confirmed - death - recovered) %>%
  mutate(active_total = cumsum(active),
         recovered_total = cumsum(recovered),
         death_total = cumsum(death))


# visualise ---------------------------------------------------------------

aus_daily_data %>% 
  ggplot() +
  geom_treemap(aes(area = active_total, fill = province),
               alpha = 0.7,
               show.legend = FALSE) + 
  geom_treemap_text(aes(area = active_total, label = province)) +
  tranisition_time(date) +
  ease_aes('linear') -> state_treemap

animate(state_treemap, length(unique(aus_daily_data$date)), .3, width = 700)


# export visual -----------------------------------------------------------

anim_save(filename = "covid_aus_state_treemap.gif")
