library(dplyr)

incarceration_data <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-jail-rates.csv?raw=true")

current_year <- max(incarceration_data$year)

recent_year_data <- incarceration_data %>%
  filter(year == current_year)

avg_total_jail_pop_rate <- mean(recent_year_data$total_jail_pop_rate, na.rm = TRUE)
avg_female_jail_pop_rate <- mean(recent_year_data$female_jail_pop_rate, na.rm = TRUE)
avg_male_jail_pop_rate <- mean(recent_year_data$male_jail_pop_rate, na.rm = TRUE)

jail_pop_rate_change <- incarceration_data %>%
  filter(year == 2018 | year == 1970) %>%
  group_by(year) %>%
  summarise(avg_jail_pop_rate = mean(total_jail_pop_rate, na.rm = TRUE))

jail_pop_rate_change <- diff(jail_pop_rate_change$avg_jail_pop_rate)

latest_data <- incarceration_data %>%
  filter(year == current_year)

avg_black_jail_pop_rate <- mean(latest_data$black_jail_pop_rate, na.rm = TRUE)
avg_white_jail_pop_rate <- mean(latest_data$white_jail_pop_rate, na.rm = TRUE)
avg_latinx_jail_pop_rate <- mean(latest_data$latinx_jail_pop_rate, na.rm = TRUE)

highest_total_jail_county <- latest_data %>%
  filter(!is.na(total_jail_pop_rate)) %>%
  arrange(desc(total_jail_pop_rate)) %>%
  slice(1) %>%
  select(state, county_name, total_jail_pop_rate, urbanicity)
