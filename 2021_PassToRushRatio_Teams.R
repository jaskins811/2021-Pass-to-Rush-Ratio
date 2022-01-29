# Load Packages
library(gt)
library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
asp_ratio <- 1.618

# Scientific Notation
options(scipen = 9999)

# Load 2021 NFL Data
data_2021 <- load_pbp(2021)

# Filter pass and rush play totals by team.
pass_to_rush <- data_2021 %>%
  filter(season_type == "REG") %>%
  filter(!is.na(posteam) & (rush == 1 | pass == 1)) %>%
  filter(wp > .2 & wp < .8 & half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarize(pass_to_rush_ratio = mean(pass), plays = n()) %>%
  arrange(-pass_to_rush_ratio)

# Create the bar graph.
pass_to_rush_plot <- pass_to_rush %>%
  left_join(teams_colors_logos, by = c("posteam" = "team_abbr")) %>%
  ggplot(aes(x = pass_to_rush_ratio, y = reorder(posteam, pass_to_rush_ratio))) +
  geom_image(aes(image = team_logo_espn), size = 0.035, by = "width", asp = asp_ratio) +
  theme_classic() +
  theme(
    panel.ticks.y = element_blank(),
    axis.text.y = element_blank()
    ) +
  scale_x_continuous(breaks = seq(0, 0.8, 0.1)) +
  labs(
    x = "Pass to Rush Ratio",
    y = "",
    title = "2021 Pass to Rush Ratio",
    subtitle = "In neutral game scripts during the regular season",
    caption = "Data: @nflfastR | Plot: Joseph Askins")

pass_to_rush_plot
repre
