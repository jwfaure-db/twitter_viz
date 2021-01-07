
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

# plot --------------------------------------------------------------------

## christmastide accidents
#### in the denominations of Western Christianity, the term "Christmas season"
#### is considered synonymous with Christmastide, which runs from December 25 
#### (Christmas Day) to January 5 (Twelfth Night or Epiphany Eve), popularly 
#### known as the 12 Days of Christmas.

xmas_dates <- c(
  dmy("25/12/2012"):dmy("05/01/2013"), 
  dmy("25/12/2013"):dmy("05/01/2014"),
  dmy("25/12/2014"):dmy("05/01/2015"),
  dmy("25/12/2015"):dmy("05/01/2016"),
  dmy("25/12/2016"):dmy("05/01/2017"),
  dmy("25/12/2017"):dmy("05/01/2018"),
  dmy("25/12/2018"):dmy("05/01/2019"),
  dmy("25/12/2019"):dmy("05/01/2020")
) %>% as_date(origin = lubridate::origin)

vic_crash_data %>% 
  mutate(major_crash = case_when(
    fatality > 0 ~ "major",
    seriousinjury > 0 ~ "major",
    TRUE ~ "minor"
  )) %>% 
  mutate(major_xmas_crash = case_when(
    accident_date %in% xmas_dates & major_crash == "major" ~ "major_xmas",
    accident_date %in% xmas_dates & major_crash == "minor" ~ "minor_xmas",
    TRUE ~ "other_crash"
  )) %>% 
  ggplot(aes(longitude, latitude, color = major_xmas_crash)) +
  geom_point(aes(alpha = major_xmas_crash, size = major_xmas_crash), 
             show.legend = FALSE) +
  scale_color_manual(values = c("#C54245", "#00873E", "gray60")) +
  scale_alpha_discrete(range = c(0.7, 0.1, 0.7)) +
  scale_size_discrete(range = c(1, 0.2, 1)) +
  ggtitle("Christmastide road accidents in Victoria", 
          subtitle = "Road accidents between 25-Dec and 5-Jan from 2012 to 2020") +
  labs(caption = "plot by Josh Faure ¦ Source: Victorian Department of Transport",
       y = NULL, x = NULL, color = NULL) +
  theme_bw() + 
  theme(
    legend.position = "none",
    plot.title = element_text(family = "Verdana", face = "bold", size = 14),
    plot.subtitle = element_markdown(lineheight = 1.2),
    plot.background = element_rect(fill = "#2B303D", color = "#2B303D"),
    panel.background = element_rect(fill = "#2B303D", color = "#8A8A8A"),
    text = element_text(family = "Verdana", color = "lightgrey"),
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) -> christmastide_plot



# export plot -------------------------------------------------------------

ggsave(christmastide_plot, filename = "plots/christmastide.png", height = 7, width = 7, units = "in")





