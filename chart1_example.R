library(ggplot2)
library(dplyr)

data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

data <- data %>%
  filter(year >= 1990) %>%
  select(year, aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, native_jail_pop_rate, white_jail_pop_rate) %>%
  mutate(across(everything(), ~ replace_na(., 0)))

agg_data <- data %>%
  group_by(year) %>%
  summarise(across(everything(), mean, na.rm = TRUE)) %>%
  pivot_longer(cols = -year, names_to = "racial_group", values_to = "incarceration_rate") %>%
  mutate(racial_group = str_replace_all(racial_group, "_jail_pop_rate", ""),
         racial_group = str_replace_all(racial_group, "_", " "),
         racial_group = str_to_title(racial_group))

ggplot(agg_data, aes(x = year, y = incarceration_rate, color = racial_group)) +
  geom_line(size = 1) +
  labs(
    title = "Incarceration Trends Over Time by Racial Group",
    x = "Year",
    y = "Incarceration Rate per 100,000 People",
    color = "Racial Group"
  )

