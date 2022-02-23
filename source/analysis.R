# A3 - analysis file

# Libraries
library("dplyr")
library("ggplot2")
library("tidyr")

# Load source data
df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

View(df)

# First area of interest - prison incarceration rate to population (15-64),
# by race, over time (all locs grouped)

prison_rate_to_pop <- df %>%
  select(year, 
         aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64,
         white_pop_15to64, aapi_prison_pop, black_prison_pop, latinx_prison_pop,
         native_prison_pop,white_prison_pop) %>%
  group_by(year) %>%
  summarise(total_aapi_pop = sum(aapi_pop_15to64),
            total_black_pop = sum(black_pop_15to64),
            total_latinx_pop = sum(latinx_pop_15to64),
            total_native_pop = sum(native_pop_15to64),
            total_white_pop = sum(white_pop_15to64),
            aapi_prison_total = sum(aapi_prison_pop, na.rm = TRUE),
            black_prison_total = sum(black_prison_pop, na.rm = TRUE),
            latinx_prison_total = sum(latinx_prison_pop, na.rm = TRUE),
            native_prison_total = sum(native_prison_pop, na.rm = TRUE),
            white_prison_total = sum(white_prison_pop, na.rm = TRUE)
            ) %>%
  filter(year %in% (1990:2016)) %>%
  mutate(
    aapi_prop = round((aapi_prison_total/total_aapi_pop) * 100, 4),
    aapi_prison_total = NULL,
    total_aapi_pop = NULL,
    black_prop = round((black_prison_total/total_black_pop) * 100,4),
    black_prison_total = NULL,
    total_black_pop = NULL,
    latinx_prop = round((latinx_prison_total/total_latinx_pop) * 100, 4),
    latinx_prison_total = NULL,
    total_latinx_pop = NULL,
    native_prop = round((native_prison_total/total_native_pop) * 100, 4),
    native_prison_total = NULL,
    total_native_pop = NULL,
    white_prop = round((white_prison_total/total_white_pop) * 100, 4),
    white_prison_total = NULL,
    total_white_pop = NULL
  ) %>%
  pivot_longer(
    cols = c(aapi_prop, black_prop, latinx_prop, native_prop, white_prop),
    names_to = "Race",
    values_to = "Proportion"
    )

# Get final values for table
final_prop_values <- prison_rate_to_pop %>%
  filter(year == 2016)

aapi_2016_prop <- final_prop_values[1]
black_2016_prop <- final_prop_values[2]
latinx_2016_prop <- final_prop_values[3]
native_2016_prop <- final_prop_values[4]
white_2016_prop <- final_prop_values[5]

View(final_prop_values)
  
prison_rate_to_pop %>%
  ggplot (aes(x = year, y = Proportion, color = Race)) +
  geom_line() +
  ggtitle("Proportion of Incarcerated People per Capita, by Race") +
  scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015)) +
  geom_point(
    data = final_prop_values,
    mapping = aes(color = Race, label = Proportion),
    alpha = 0.8
    ) +
  geom_label(
    data = final_prop_values,
    aes(label = paste0(round(Proportion,2),"%")),
    alpha = 0.75,
    nudge_y = 0.08,
    show.legend = FALSE
    )

View(prison_rate_to_pop)

# Connection between counties that jail kids at high rate and ___?
# Jail capacity?
# Counties with highest incarcerated per pop?
# Admissions over time, maybe