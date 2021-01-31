
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# NFL average score over time 
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(nflscrapR)
library(tidybayes)


# theming -----------------------------------------------------------------

theme_set(theme_minimal(base_family = "Oswald"))

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


# load data ---------------------------------------------------------------

first <- 2009 #first season to grab. min available=2009
last <- 2019 # most recent available season
seasons <- seq(first, last, by = 1)

reg_season_data = list()
post_season_data = list()

for(i in seasons){
  rs_url <- paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/regular_season/reg_games_", i, ".csv")
  ps_url <- paste0("https://raw.githubusercontent.com/ryurko/nflscrapR-data/master/games_data/post_season/post_games_", i, ".csv")
  
  reg_season_data[[i]] <- read_csv(rs_url)
  post_season_data[[i]] <- read_csv(ps_url)
}

reg_season <- bind_rows(reg_season_data)
post_season <- bind_rows(post_season_data)

# data manipulation -------------------------------------------------------

## tidy data into long format
tidy_rs_scoring <- reg_season %>% 
  select(-away_team, -away_score) %>% 
  rename(team = home_team,
         score = home_score) %>% 
  bind_rows(reg_season %>% 
              select(-home_team, -home_score) %>% 
              rename(team = away_team,
                     score = away_score)) %>% 
  drop_na()

## tidy data into long format
tidy_ps_scoring <- post_season %>% 
  select(-away_team, -away_score) %>% 
  rename(team = home_team,
         score = home_score) %>% 
  bind_rows(post_season %>% 
              select(-home_team, -home_score) %>% 
              rename(team = away_team,
                     score = away_score)) %>% 
  drop_na()

## keep only winning scores
tidy_winners <- reg_season %>% 
  rowwise() %>% 
  mutate(winning_score = max(c(home_score, away_score), na.rm = TRUE))


# data viz ----------------------------------------------------------------

plot <- tidy_winners %>% 
  ggplot(aes(x = winning_score, y = season)) +
  stat_interval(.width = c(.1, .25, .5, .75, 1),
                height = 5, show.legend = F, 
                orientation = "horizontal") +
  rcartocolor::scale_color_carto_d(palette = "Mint") + 
  stat_halfeye(aes(winning_score, season + 0.06),
               .width = 0, fill = "#CFFFE5", alpha = 0.2, height = 0.7,
               size = 0.7, point_alpha = 1, point_color = "#590000",
               orientation = "horizontal", na.rm = TRUE) +
  coord_flip() +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 65), 
                     breaks = seq(0, 65, by = 5)) + 
  scale_y_continuous(expand = c(0, 0), breaks = seasons) +
  labs(
    title = "Are NFL offenses more prolific?",
    subtitle = "Range and distribution of winning score in NFL regular season matches from 2009 to 2019",
    caption = "\n visualization by Josh Faure  ¦  data: NFL.com",
    x = NULL,
    y = NULL
  )

legend_text <- 
  tibble(
    xt = c(5, 4.125, 3.125, 1.875, 0.625, 7.5),
    yt = rep(1.02, 6),
    text = c("10%", "25%", "50%", "75%", "100%", "of winning scores fall in this range")
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
