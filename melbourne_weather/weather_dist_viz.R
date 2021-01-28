
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# Melbourne weather over time 
# http://www.bom.gov.au/climate/data/
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(rvest)
library(tidybayes)

source("R_rainclouds.R")


# theming -----------------------------------------------------------------

theme_set(theme_minimal(base_family = "Montserrat"))

theme_update(
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(size = 0.1),
  axis.text.x = element_text(size = 11, face = "bold"),
  axis.text.y = element_text(size = 9, color = "grey65"),
  plot.title = element_text(size = 26, hjust = 0, face = "bold"),
  plot.subtitle = element_text(size = 13, hjust = 0, color = "grey65"),
  plot.caption = element_text(size = 10, color = "grey65")
)


# scrape data -------------------------------------------------------------

## create tibble of years and webpages to scrape
climate_data <- tibble(year = 1980:2020) %>% 
  mutate(page = paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=122&p_display_type=dailyDataFile&p_startYear=", year, "&p_c=-1514873801&p_stn_num=087031"))

## function to scrape max temp tables
get_tables <- function(page){
  read_html(page) %>%
    html_node("table") %>% 
    html_table(header = FALSE, fill = TRUE)
}


## scrape the links for tables
climate_data <- climate_data %>% 
  mutate(temp_table = map(page, get_tables))

climate_data <- climate_data %>% 
  unnest(cols = c(temp_table)) %>% 
  mutate(X1 = parse_number(X1)) %>% 
  filter(!is.na(X1), X1 < 32) 

write_csv(climate_data, "melbourne_weather_data.csv")

# data manipulation -------------------------------------------------------

## change col names to get months
hdr <- c("Day", "Jan", "Feb", "Mar", "Apr", "May", "Jun", 
         "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
oldnames <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", 
              "X8", "X9", "X10", "X11", "X12", "X13")

## tidy data
plot_df <- climate_data %>% 
  rename_with(~ hdr[which(oldnames == .x)], .cols = oldnames) %>% 
  select(-page) %>% 
  pivot_longer(Jan:Dec, 
               names_to = "Month", 
               values_to = "Max Temp") %>% 
  janitor::clean_names() %>% 
  mutate(
    month = match(month, month.abb),
    date = lubridate::make_date(year, month, day),
    max_temp = as.numeric(max_temp),
    year = lubridate::year(date),
    month = lubridate::month(date)
  ) %>% 
  drop_na()


# data viz ----------------------------------------------------------------

plot <- plot_df %>% 
  filter(year >= 2000) %>% 
  ggplot(aes(x = max_temp, y = month)) +
  stat_interval(.width = c(.1, .25, .5, .75, 1),
                height = 5, show.legend = F, 
                orientation = "horizontal") +
  rcartocolor::scale_color_carto_d(palette = "Magenta") + 
  stat_halfeye(aes(max_temp, month+0.06),
                .width = 0, fill = "tan", alpha = 0.2, height = 0.7,
                size = 0.7, point_alpha = 1, point_color = "#590000",
               orientation = "horizontal", na.rm = TRUE) +
  coord_flip(ylim = c(0.5, 13)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 50), 
                     breaks = seq(0, 50, by = 5), 
                     labels = glue::glue("{seq(0, 50, by = 5)}°C")) + 
  scale_y_continuous(expand = c(0, 0), breaks = 1:12,
                     labels = c("Jan", 
                                "Feb",
                                "Mar",
                                "Apr",
                                "May",
                                "Jun",
                                "Jul",
                                "Aug",
                                "Sep",
                                "Oct",
                                "Nov",
                                "Dec")) +
  labs(
    title = "Daily Temperatures in Melbourne, Australia",
    subtitle = "Range and distribution of maximum daily temperatures in Celsius per month from 2000 to 2020 measured in Melbourne, Australia",
    caption = "\n visualization by Josh Faure  ¦  data: BOM (Bureau of Meteorology)",
    x = NULL,
    y = NULL
  )


legend_text <- 
  tibble(
    xt = c(5, 4.125, 3.125, 1.875, 0.625, 7.5),
    yt = rep(1.02, 6),
    text = c("10%", "25%", "50%", "75%", "100%", "of measured temperatures fall in this range")
  )

legend <- ggplot(data = tibble(x = 0:10, y = rep(1, 11)), aes(x, y)) + 
  stat_interval(.width = c(.1, .25, .5, .75, 1), show.legend = F, 
                orientation = "horizontal") +
  rcartocolor::scale_color_carto_d(palette = "Magenta") +
  coord_cartesian(ylim = c(0.9, 1.1)) +
  geom_text(data = legend_text, aes(xt, yt, label = text), 
            family = "Montserrat", color = "grey65", size = 3) +
  theme_void()

ggdraw(plot) +
  draw_plot(legend, .275, .01, .525, .3)

# export plot -------------------------------------------------------------

ggsave("plots/melb_weather.png", width = 13, height = 8)
