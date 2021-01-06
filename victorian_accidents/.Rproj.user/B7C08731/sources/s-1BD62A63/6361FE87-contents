# by josh faure

library(tidyverse)
library(reshape2)
library(readxl)
library(corrplot)
library(ggpubr)

# import data
# data source: https://www.crimestatistics.vic.gov.au/crime-statistics/latest-victorian-crime-data/download-data
criminal_incidents <- readxl::read_excel("Data_Tables_Criminal_Incidents_Visualisation_Year_Ending_September_2020.xlsx",
  sheet = "Table 03"
) %>%
  janitor::clean_names() %>%
  janitor::remove_empty()

# tidy data for visualisation
ci_df <- criminal_incidents %>%
  filter(offence_division != "F Other offences") %>%
  transmute(year,
    offence_subdivision = offence_subdivision %>% str_replace_all("[^[:alnum:]]", " "),
    family_incident = str_remove(family_incident_flag, " related"),
    incidents = incidents_recorded,
    rate = rate_per_100_000_population
  )

# prepare plot on samples ---------------------------------------------------------------------
ci_df %>%
  filter(offence_subdivision == "A20 Assault and related offences") %>%
  ggplot() +
  geom_line(aes(x = year, y = rate, color = family_incident, group = family_incident),
    show.legend = FALSE,
    alpha = .5,
    size = 3
  ) +
  scale_color_manual(values = c("cyan", "orange")) +
  scale_fill_manual(values = c("cyan", "orange")) +
  labs(title = "Sexual Offence",
       y = "Rate of Offences",
       x = "Year") +
  theme_bw() +
  theme(
    legend.position = c(.9, .9),
    legend.background = element_rect(fill = "transparent"),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "#2B303D", color = "#2B303D"),
    panel.background = element_rect(fill = "#2B303D", color = "#8A8A8A"),
    text = element_text(color = "lightgrey"),
    axis.text = element_text(color = "lightgrey", size = 6),
    panel.grid.major = element_line(color = "#8A8A8A"),
    panel.grid.minor = element_line(color = "#8A8A8A"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 10, hjust = .5),
    axis.title = element_text(size = 8),
  ) +
  rotate_x_text() -> test
test

# for loop to create plots for each offense -------------------------------
iterator <- ci_df %>%
  count(offence_subdivision) %>%
  pull(offence_subdivision)
for (i in iterator) {
  ttl = paste(i %>% str_sub(start = 5) %>% str_to_title())
  ci_df %>%
    filter(offence_subdivision == i) %>%
    ggplot() +
    geom_line(aes(x = year, y = rate, color = family_incident, group = family_incident),
      show.legend = FALSE,
      alpha = .5,
      size = 2
    ) +
    scale_color_manual(values = c("cyan", "orange")) +
    scale_fill_manual(values = c("cyan", "orange")) +
    labs(title = paste(strwrap(ttl, width = 30), collapse = "\n"),
         y = "Rate of Offences",
         x = "Year") +
    theme_bw() +
    theme(
      legend.position = c(.9, .9),
      legend.background = element_rect(fill = "transparent"),
      legend.title = element_blank(),
      plot.background = element_rect(fill = "#2B303D", color = "#2B303D"),
      panel.background = element_rect(fill = "#2B303D", color = "#8A8A8A"),
      text = element_text(color = "white"),
      axis.text = element_text(color = "white", size = 6),
      panel.grid.major = element_line(color = "#8A8A8A"),
      panel.grid.minor = element_line(color = "#8A8A8A"),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(size = 8, hjust = .5),
      axis.title = element_text(size = 8),
    ) +
    rotate_x_text() -> test
  ggsave(test, filename = paste("plots/", i, ".png", sep = ""), width = 6, height = 6, units = "cm")
}

