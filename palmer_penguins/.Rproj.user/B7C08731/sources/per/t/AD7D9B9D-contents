# script by Josh Faure
# jan 2021
# jwfaure.github.io

library(dplyr)
library(palmerpenguins)
library(ggtext)

theme_set(theme_bw())
set.seed(73)
`%notin%` <- Negate(`%in%`)

source("R_rainclouds.R")

# responding to Tobias Stalder regarding visualizations hiding information
# based on the palmer penguins datasets


penguins %>%
  group_by(species) %>%
  ggplot(aes(x = species, y = body_mass_g, colour = species, fill = species)) +
  geom_flat_violin(
    position = position_nudge(x = 0.25, y = 0),
    alpha = 0.4, adjust = 2, trim = FALSE
  ) +
  geom_point(
    position = position_jitter(width = .15),
    size = .4, alpha = 0.7, na.rm = TRUE
  ) +
  geom_boxplot(aes(x = as.numeric(species) + 0.25, y = body_mass_g),
    width = 0.1, outlier.shape = NA, alpha = 0.8, na.rm = TRUE
  ) +
  scale_colour_manual(values = c("#ff7400", "#c75bcb", "#067476")) +
  scale_fill_manual(values = c("#ff7400", "#c75bcb", "#067476")) +
  scale_y_continuous(name = "Body mass (grams)") +
  scale_x_discrete(name = "Penguin species") +
  labs(
    title = "Distribution of penguin body mass (g) by species",
    subtitle = "The use of raincloud plots to show complete picture",
    caption = "**Plot by** Josh Faure<br>
       *jwfaure.github.io*<br><br>
              **Inspired by:** 'What are you hiding with your summary statistics?',<br>
       Illustration by Tobias Stalder. *tobias-stalder.netlify.app*<br><br>
       **Data:** Horst AM, Hill AP, Gorman KB (2020). palmerpenguins: <br>Palmer
       Archipelago (Antarctica) penguin data. R package version 0.1.0.<br><br>
       **Packages:** {palmerpenguins}, {dplyr}, {ggplot2}, {ggtext}"
  ) +
  guides(fill = FALSE, colour = FALSE) +
  ggtitle("Distribution of penguin body mass (g) by species") +
  theme(
    legend.position = "none",
    plot.caption = element_markdown(hjust = 0, color = "#909497", size = 6),
    panel.border = element_blank(),
    axis.line = element_line(color = "#a6acaf"),
    plot.background = element_rect(fill = "#303841", color = "#303841"),
    panel.background = element_rect(fill = "#303841", color = "#303841"),
    text = element_text(color = "#a6acaf"),
    axis.text = element_text(color = "#909497"),
    panel.grid = element_blank()
  ) +
  ggsave(filename = "palmer_penguins.png", dpi = 300)
