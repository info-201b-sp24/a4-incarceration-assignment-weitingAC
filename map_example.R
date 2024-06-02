library(dplyr)
library(ggplot2)
library(maps)
library(patchwork)

incarceration_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true", stringsAsFactors = FALSE)

incarceration_data <- incarceration_data %>%
  mutate(urbanicity_numeric = factor(urbanicity, levels = c("rural", "small/mid", "suburban", "urban"), labels = c(0, 1, 2, 3))) %>%
  filter(!is.na(total_jail_pop_rate) & !is.na(urbanicity))

average_rates <- incarceration_data %>%
  group_by(state, urbanicity) %>%
  summarize(average_rate = mean(total_jail_pop_rate, na.rm = TRUE), .groups = 'drop') %>%
  arrange(state, urbanicity) %>%
  group_by(state) %>%
  mutate(change_in_rate = c(NA, diff(average_rate))) %>%
  filter(!is.na(change_in_rate)) %>%
  summarize(avg_change = mean(change_in_rate, na.rm = TRUE), .groups = 'drop')

state_changes <- average_rates %>%
  left_join(data.frame(state = state.abb, region = tolower(state.name)), by = "state")

plot_data <- map_data("state") %>%
  left_join(state_changes, by = "region") %>%
  arrange(order)

ggplot(plot_data, aes(x = long, y = lat, group = group, fill = avg_change)) +
  geom_polygon(color = "black") +
  coord_fixed(1.3) +
  scale_fill_gradient2(low = "blue", mid = "gray", high = "red", midpoint = 0,
                       name = "Avg Change in\nIncarceration Rate",
                       na.value = "orange") +
  labs(title = "Incarceration Rate Change Across Urbanicity Levels by State",
       fill = "Avg Change in\nIncarceration Rate") +
  theme_minimal() +
  theme(legend.position = "right",
        legend.box = "vertical",
        plot.title = element_text(hjust = 0.5, size = 14),
        legend.title = element_text(hjust = 0.5)) +
  guides(fill = guide_colorbar(title.position = "top"),
         color = guide_legend(title = NULL, override.aes = list(shape = 15, size = 5)))
