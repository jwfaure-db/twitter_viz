
# info --------------------------------------------------------------------
# 
# script by Josh Faure
# Jan 2021
# Google trends for NBA players over time 
# jwfaure.github.io



# load libraries ----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gtrendsR)
library(patchwork)
library(ggtext)
library(wesanderson)


# theming -----------------------------------------------------------------

theme_set(theme_bw(base_family = "Montserrat"))

theme_update(
  legend.position = "none",
  plot.background = element_rect(fill = "#222222"),
  plot.title = element_text(colour = "#f8c879", 
                            size=22, 
                            face="bold",
                            hjust = 0.5),
  plot.subtitle = element_markdown(colour = "#f8c879", 
                               size=12,
                               hjust = 0.5),
  plot.caption = element_text(colour = "#f8c879", size=10),
  panel.background = element_rect(fill = "#222222"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  strip.background = element_rect(fill = "#222222", colour = NA),
  strip.text.y.left = element_text(colour = "#f8c879",
                            size=10,
                            angle = 0, vjust = 0.5, hjust = 1)
)


# load data ---------------------------------------------------------------

historic_players <- c(
  "Michael Jordan",
  "Kobe Bryant"
)

current_players <- c(
  "LeBron James",
  "James Harden",
  "Kyrie Irving",
  "Kevin Durant",
  "Steph Curry"
)

all_players <- c(historic_players, current_players)

historic <- gtrends(keyword = historic_players,
        time = "all",
        onlyInterest = TRUE) %>% 
  plyr::ldply(data.frame)

current <- gtrends(keyword = current_players,
                   time = "all",
                   onlyInterest = TRUE) %>% 
  plyr::ldply(data.frame)


# data manipulation -------------------------------------------------------

interest_df <- historic %>% 
  bind_rows(current) %>% 
  as_tibble() %>% 
  transmute(date = ymd(date),
            hits = parse_number(hits),
            keyword)


# plot --------------------------------------------------------------------

interest_df %>% 
  ggplot(aes(x = date, y = hits, colour = hits)) +
  geom_line(size = 1.2, alpha = 0.6) +
  scale_colour_gradientn(colours = wes_palette("Zissou1", type = "continuous")) + 
  facet_wrap(~ keyword, ncol = 1, strip.position = "left") +
  labs(title = "Popularity of franchise basketball players",
       subtitle = "From <span style='color:#3B9AB2'>least searched</span> to <span style='color:#F21A00'>most searched</span>",
       caption = "visualization by Josh Faure  ¦  data: trends.google.com")


# export plot -------------------------------------------------------------

ggsave("plots/nba_google_trends.png", height = 18, width = 12, units = "in")
