library(dplyr)
library(ggplot2)

# Load and filter the data
data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

# Filter to the relevant years and replace NA values
data <- data %>%
  filter(year >= 1990) %>%
  select(year, aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, native_jail_pop_rate, white_jail_pop_rate) %>%
  mutate(across(everything(), ~ replace_na(., 0)))

# Plot the data
ggplot() +
  labs(
    title = "Incarceration Trends Over Time by Racial Group",
    x = "Year",
    y = "Incarceration Rate per 100,000 People",
    color = "Racial Group"
  ) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 24)) +
  geom_line(data = data, aes(x = year, y = aapi_jail_pop_rate, color = "AAPI")) +
  geom_line(data = data, aes(x = year, y = black_jail_pop_rate, color = "Black")) +
  geom_line(data = data, aes(x = year, y = latinx_jail_pop_rate, color = "Latinx")) +
  geom_line(data = data, aes(x = year, y = native_jail_pop_rate, color = "Native")) +
  geom_line(data = data, aes(x = year, y = white_jail_pop_rate, color = "White")) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 8)
  )
