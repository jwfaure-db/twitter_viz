
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# Australian covid cases
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(coronavirus)
library(rvest)
library(geofacet)
library(gganimate)
library(ggtext)
library(RColorBrewer)

# theming -----------------------------------------------------------------

theme_set(theme_minimal(base_family = "Oswald"))

theme_update(
  legend.position = "none",
  plot.title = element_text(size = 27,
                            color = "grey30",
                            face = "bold",
                            hjust = .5),
  plot.subtitle = element_text(size = 15,
                            color = "grey40",
                            face = "bold",
                            hjust = .5),
  plot.caption = element_text(size = 9,
                              color = "grey40",
                              hjust = .5),
  axis.text  = element_text(color = "#909497"),
  axis.title = element_blank(),
  axis.line = element_line(color = "#a6acaf"),
  plot.background = element_rect(fill = "grey88", color = NA),
  panel.background = element_rect(fill = NA, color = NA),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  strip.text = element_text(color = "grey40", face = "bold"),
  plot.margin = margin(rep(20, 4))
)


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

# obtain state populations downloaded datacube from ABS
aus_population <- read_csv("aus_population.csv")

# merge data together to obtain rates of covid
aus_daily_data <- aus_daily_data %>% 
  left_join(aus_population, by = "province") %>% 
  transmute(
    province,
    date,
    active_total, 
    active_total_rate = 100000 * active_total / population
  )

## modifications geofacet
my_grid <- aus_grid1
my_grid$row[my_grid$code == "TAS"] <- 4


# visualise ---------------------------------------------------------------

aus_daily_data %>% 
  ggplot(aes(x = date, y = active_total_rate, fill = province)) +
  geom_area(alpha = 0.7, show.legend = FALSE) + 
  # geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") +
  facet_geo(~ province, grid = my_grid) +
  labs(x = NULL, y = NULL, 
       title = "COVID-19 ACROSS AUSTRALIA",
       subtitle = "RATE OF TOTAL ACTIVE CASES PER 100K POPULATION",
       caption = "visualization by Josh Faure  ¦  data: https://github.com/RamiKrispin/coronavirus") +
  transition_reveal(date) -> state_treemap

animate(state_treemap, 
        res = 100, 
        height = 7, width = 7, units = "in")


# export visual -----------------------------------------------------------

anim_save(filename = "covid_aus_state_treemap.gif")
