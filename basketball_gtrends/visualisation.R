
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
library(gtable)
library(ggtext)


# theming -----------------------------------------------------------------

theme_set(theme_bw(base_family = "PT Sans"))

theme_update(
  legend.position = "none",
  plot.background = element_rect(fill = "#17263C"),
  plot.title = element_text(colour = "#C3ECB2", 
                            size=18, 
                            face="bold",
                            hjust = 0.5),
  plot.subtitle = element_markdown(colour = "#C3ECB2", 
                               size=9,
                               hjust = 0.5),
  plot.caption = element_text(colour = "#C3ECB2", size=9),
  panel.background = element_rect(fill = "#17263C"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  axis.line = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  axis.title.x = element_blank(),
  axis.title.y = element_text(colour = "#C3ECB2",
                              angle = 90, vjust = 0.5, hjust = 1)
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

plots <- vector(mode = "list")

for(k in unique(interest_df$keyword)){
  plots[[k]] <- interest_df %>% 
    filter(keyword == k) %>% 
    ggplot(aes(x = date, y = hits, colour = hits)) +
    geom_line(size = 1.2, alpha = 0.7) +
    scale_colour_viridis_c(option = "inferno") + 
    scale_y_continuous(name = paste0(k), 
                       expand = c(0,0))
}

gridExtra::grid.arrange(grobs = plots, 
                        ncol = 1) 
# + 
#   labs(title = "Popularity of franchise basketball players", 
#        subtitle = "From <span style='color:#000004'>least searched</span> to <span style='color:#FCFFA4'>most searched</span>",
#        caption = "visualization by Josh Faure  ¦  data: trends.google.com",
#        x = NULL,
#        y = NULL)


# export plot -------------------------------------------------------------

png("plots/nba_google_trends.png")
gridExtra::grid.arrange(grobs = plots, 
                        ncol = 1) 
dev.off()
