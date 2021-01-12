
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# Australian transit costs #tidyduesday
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(patchwork)
library(rvest)
library(gghighlight)
library(ggtext)

# load data ---------------------------------------------------------------

tt <- tidytuesdayR::tt_load(2021, week = 2)
transit_cost <- tt$transit_cost

# data manipulation -------------------------------------------------------

country_exp <- transit_cost %>% 
  select(country, city, start_year, end_year, length, real_cost) %>% 
  drop_na() %>% 
  filter(end_year != "X") %>% 
  mutate(start_year = as.integer(start_year),
         end_year = as.integer(end_year),
         real_cost = as.numeric(real_cost),
         duration = end_year - start_year,
         city = str_to_upper(city)) %>% 
  group_by(country) %>% 
  summarise(exp_length = sum(length) / sum(duration),
            exp_cost = sum(real_cost) / sum(length)) %>% 
  ungroup()

# obtain country code names
country_exp <- read_html("https://www.iban.com/country-codes") %>%  
  html_nodes(xpath = '/html/body/div[1]/div[2]/div/div/div/div/table') %>% 
  html_table() %>% 
  .[[1]] %>% 
  select(`Country`, `Alpha-2 code`) %>% 
  right_join(country_exp, by = c("Alpha-2 code" = "country")) %>% 
  select(-`Alpha-2 code`) %>% 
  filter(Country != "NA")


# plot --------------------------------------------------------------------

p1 <- country_exp %>% 
  mutate(Country = fct_reorder(Country, exp_cost)) %>%
  ggplot() +
  geom_col(aes(exp_cost, Country), fill = "orange") + 
  gghighlight(Country == "Australia",
              unhighlighted_params = list(fill = "gray40"), 
              use_group_by = FALSE,
              use_direct_label = FALSE) +
  scale_x_continuous(trans = "reverse", breaks = c(0, 1000, 2000), labels=scales::dollar_format()) +
  scale_y_discrete(position = "right") +
  labs(x = "# Expected cost (million) per km",
       y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(family = "Verdana", face = "bold", color = "white"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(family = "Verdana", face = "bold", color = "white"),
    axis.title.y = element_blank(),
    panel.grid = element_line(color = "#85929E"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

p2 <- country_exp %>% 
  mutate(Country = fct_reorder(Country, exp_length)) %>%
  ggplot() +
  geom_col(aes(exp_length, Country), fill = "orange") + 
  gghighlight(Country == "Australia",
              unhighlighted_params = list(fill = "gray40"), 
              use_group_by = FALSE,
              use_direct_label = FALSE) + 
  scale_x_continuous(breaks = c(0, 15, 30), labels = scales::unit_format(unit = "km")) +
  labs(x = "# Expected progress per year") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(family = "Verdana", face = "bold", color = "white"),
    axis.text.y = element_blank(),
    axis.title.x = element_text(family = "Verdana", face = "bold", color = "white"),
    axis.title.y = element_blank(),
    panel.grid = element_line(color = "#85929E"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
  )

(p1 + p2) +
  plot_annotation(
    title = "TRANSIT PROJECTS AROUND THE WORLD \n INFRASTRUCTURE COSTS AND CONSTRUCTION SPEEDS",
    subtitle = "WHERE <span style='color:orange'>AUSTRALIA</span> RANKS AMONGST THE REST OF THE WORLD",
    caption = "\n #tidyTuesday ¦ visualization by Josh Faure",
    theme = theme(
      plot.title = element_text(hjust = 0.5, family = "Verdana", face = "bold", size = 26, colour = "white"),
      plot.subtitle = element_markdown(hjust = 0.5, family = "Verdana", face = "bold", colour = "white"),
      plot.caption = element_text(hjust = 0.5, family = "Verdana", face = "bold", colour = "white"),
      plot.background = element_rect(color = "#363636", fill = "#363636"),
      plot.margin = margin(3, 3, 1, 3, unit = "cm"),
      panel.background = element_rect(color = "transparent", fill = "#474747"),
    )
  )


# export plot -------------------------------------------------------------

ggsave("plots/aus_transit_costs.png", height = 12, width = 18, units = "in")
