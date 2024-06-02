library(dplyr)
library(ggplot2)

incarceration_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

incarceration_data <- incarceration_data %>%
  mutate(urbanicity_cont = case_when(
    urbanicity == "rural" ~ 0,
    urbanicity == "small/mid" ~ 1,
    urbanicity == "suburban" ~ 2,
    urbanicity == "urban" ~ 3,
    TRUE ~ NA_real_
  ))

avg_incarceration_rates <- incarceration_data %>%
  group_by(urbanicity_cont) %>%
  summarise(across(starts_with("aapi_jail_pop_rate"):starts_with("white_jail_pop_rate"), ~ mean(.x, na.rm = TRUE), .names = "avg_{col}")) %>%
  pivot_longer(cols = starts_with("avg_"), names_to = "ethnicity", values_to = "avg_rate") %>%
  mutate(ethnicity = str_replace(ethnicity, "avg_", ""))

ggplot(avg_incarceration_rates, aes(x = urbanicity_cont, y = avg_rate, color = ethnicity)) +
  geom_line(size = 1) +
  labs(
    title = "Average Incarceration Rates by Urbanicity & Ethnicity",
    x = "Urbanicity",
    y = "Average Incarceration Rate",
    color = "Ethnic Group"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 0:3, labels = c("Rural", "Small/Mid", "Suburban", "Urban")) +
  scale_color_discrete(labels = c("AAPI", "Black", "Latinx", "Native", "White"))

