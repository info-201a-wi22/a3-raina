# A3 - analysis file
###############################################################################
# Libraries
library("dplyr")
library("ggplot2")
library("tidyr")
library("leaflet")
library("tigris")

# Load source data
df <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv", stringsAsFactors = FALSE)

# Summary stat
total_incarcerated_2016 <- df %>%
  select(year, total_jail_pop, total_prison_pop) %>%
  group_by(year) %>%
  summarise(
    total_incarcerated = round(sum(total_jail_pop, na.rm = TRUE) + sum(total_prison_pop, na.rm = TRUE))) %>%
  filter(year == 2016) %>%
  pull(total_incarcerated)

total_incarcerated_2016 <- prettyNum(total_incarcerated_2016, big.mark=",", scientific = FALSE)
###############################################################################
# Data for Time Series

# Incarceration rate to population (15-64), by race, over time 

# Group on urbanicity
prison_rate_to_pop <- df %>%
  select(year, 
         aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64,
         white_pop_15to64, aapi_prison_pop, black_prison_pop, latinx_prison_pop,
         native_prison_pop,white_prison_pop, aapi_jail_pop, black_jail_pop, 
         latinx_jail_pop,native_jail_pop,white_jail_pop) %>%
  group_by(year) %>%
  summarise(
    total_aapi_pop = sum(aapi_pop_15to64),
    total_black_pop = sum(black_pop_15to64),
    total_latinx_pop = sum(latinx_pop_15to64),
    total_native_pop = sum(native_pop_15to64),
    total_white_pop = sum(white_pop_15to64),
    aapi_incar_total = 
      sum(aapi_prison_pop, na.rm = TRUE) +
      sum(aapi_jail_pop, na.rm = TRUE),
    black_incar_total =
      sum(black_prison_pop, na.rm = TRUE) +
      sum(black_jail_pop, na.rm = TRUE),
    latinx_incar_total =
      sum(latinx_prison_pop, na.rm = TRUE) +
      sum(latinx_jail_pop, na.rm = TRUE),
    native_incar_total =
      sum(native_prison_pop, na.rm = TRUE) +
      sum(native_jail_pop, na.rm = TRUE),
    white_incar_total =
      sum(white_prison_pop, na.rm = TRUE) +
      sum(white_jail_pop, na.rm = TRUE),
  ) %>%
  filter(year %in% (1990:2016)) %>%
  mutate(
    aapi_prop = round((aapi_incar_total/total_aapi_pop) * 100, 4),
    black_prop = round((black_incar_total/total_black_pop) * 100,4),
    latinx_prop = round((latinx_incar_total/total_latinx_pop) * 100, 4),
    native_prop = round((native_incar_total/total_native_pop) * 100, 4),
    white_prop = round((white_incar_total/total_white_pop) * 100, 4)
  ) %>%
  select(year, aapi_prop, black_prop, latinx_prop, native_prop, white_prop) %>%
  pivot_longer(
    cols = c(aapi_prop, black_prop, latinx_prop, native_prop, white_prop),
    names_to = "Race",
    values_to = "Proportion"
  )

# Get final values for table
final_prop_values <- prison_rate_to_pop %>%
  filter(year == 2016)

# Get val for Black and white people for summary
black_incarcerated_2016 <- pull(round(final_prop_values[2,3], 2))
white_incarcerated_2016 <- pull(round(final_prop_values[5,3], 2))

###############################################################################
# Add urbanicity - all you need to do is filter out urban values before the rest
rural_prison_rate_to_pop <- df %>%
  select(year, urbanicity,
         aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64,
         white_pop_15to64, aapi_prison_pop, black_prison_pop, latinx_prison_pop,
         native_prison_pop,white_prison_pop, aapi_jail_pop, black_jail_pop, 
         latinx_jail_pop,native_jail_pop,white_jail_pop) %>%
  filter(urbanicity != "urban") %>%
  filter(urbanicity != "suburban") %>%
  group_by(year) %>%
  summarise(total_aapi_pop = sum(aapi_pop_15to64),
            total_black_pop = sum(black_pop_15to64),
            total_latinx_pop = sum(latinx_pop_15to64),
            total_native_pop = sum(native_pop_15to64),
            total_white_pop = sum(white_pop_15to64),
            aapi_incar_total = 
              sum(aapi_prison_pop, na.rm = TRUE) + sum(aapi_jail_pop, na.rm = TRUE),
            black_incar_total =
              sum(black_prison_pop, na.rm = TRUE) + sum(black_jail_pop, na.rm = TRUE),
            latinx_incar_total =
              sum(latinx_prison_pop, na.rm = TRUE) + sum(latinx_jail_pop, na.rm = TRUE),
            native_incar_total =
              sum(native_prison_pop, na.rm = TRUE) + sum(native_jail_pop, na.rm = TRUE),
            white_incar_total =
              sum(white_prison_pop, na.rm = TRUE) + sum(white_jail_pop, na.rm = TRUE),
  ) %>%
  filter(year %in% (1990:2016)) %>%
  mutate(
    aapi_prop = round((aapi_incar_total/total_aapi_pop) * 100, 4),
    black_prop = round((black_incar_total/total_black_pop) * 100,4),
    latinx_prop = round((latinx_incar_total/total_latinx_pop) * 100, 4),
    native_prop = round((native_incar_total/total_native_pop) * 100, 4),
    white_prop = round((white_incar_total/total_white_pop) * 100, 4)
  ) %>%
  select(year, aapi_prop, black_prop, latinx_prop, native_prop, white_prop) %>%
  pivot_longer(
    cols = c(aapi_prop, black_prop, latinx_prop, native_prop, white_prop),
    names_to = "Race",
    values_to = "Proportion"
  )

# For clarity in time series
rural_final_prop_values <- rural_prison_rate_to_pop %>%
  filter(year == 2016)

# For summary
rural_black_incarcerated_2016 <- pull(round(rural_final_prop_values[2,3], 2))

###############################################################################
# TIME SERIES

# all values
proportion_incarcerated <- prison_rate_to_pop %>%
  ggplot (aes(x = year, 
              y = Proportion, 
              color = Race)) +
  geom_line() +
  ggtitle("Proportion of Incarcerated People per Capita, by Race") +
  scale_x_discrete(breaks = c(1990,1995,2000,2005,2010,2015)) +
  geom_point(
    data = final_prop_values,
    mapping = aes(color = Race, label = Proportion),
    alpha = 0.8
    ) +
  geom_label(
    data = final_prop_values,
    aes(label = paste0(round(Proportion,2),"%")),
    alpha = 0.75,
    nudge_y = 0.1,
    show.legend = FALSE
    )

# rural values
rural_proportion_incarcerated <- rural_prison_rate_to_pop %>%
  ggplot (aes(x = year, 
              y = Proportion, 
              color = Race)) +
  geom_line() +
  ggtitle("Proportion of Incarcerated People per Capita, by Race in Rural Counties") +
  scale_x_discrete(breaks = c(1990,1995,2000,2005,2010,2015)) +
  geom_point(
    data = rural_final_prop_values,
    mapping = aes(color = Race, label = Proportion),
    alpha = 0.8
  ) +
  geom_label(
    data = rural_final_prop_values,
    aes(label = paste0(round(Proportion,2),"%")),
    alpha = 0.75,
    nudge_y = 0.1,
    show.legend = FALSE
  )

###############################################################################
# highest incarceration rate of Black folks per state, separated by urbanicity
highest_incar_county_per_state <- df %>%
  filter(year == 2016) %>%
  select(
    fips, state, county_name, black_pop_15to64, black_jail_pop, black_prison_pop,
    urbanicity
  ) %>%
  mutate(
    total_incarcerated = black_jail_pop + black_prison_pop,
    proportion_incarcerated = round((total_incarcerated/black_pop_15to64) * 100, 2)
  ) %>%
  select(fips, state, county_name, proportion_incarcerated, urbanicity) %>%
  top_n(n = 60, wt = proportion_incarcerated) %>%
  arrange(-proportion_incarcerated)

# Need county boundaries
county_FIPS <- as.character(highest_incar_county_per_state$county_FIPS) 

# Make vector of states for shapefile
state_names <- highest_incar_county_per_state %>%
  select(state) %>%
  distinct(state) %>%
  pull(state)

county_shapes <- counties(state = state_names, year = 2016)

county_FIPS <- highest_incar_county_per_state %>%
  select(fips, urbanicity, state, proportion_incarcerated) %>%
  mutate(
    state_fips = substring(fips,1,2),
    county_fips = substring(fips,3,5)
    ) %>%
  select(-fips)

counties_shapefile <- left_join(
  county_FIPS,
  county_shapes,
  by = c("county_fips" = "COUNTYFP", "state_fips" = "STATEFP"))

# Color by urbanicity
urbanicity_color <- colorFactor(palette = "Dark2", counties_shapefile$urbanicity)

###### Map
highest_incar_county_map <- leaflet(counties_shapefile) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons( 
              data = counties_shapefile$geometry,
              stroke = T,
              fillColor = urbanicity_color(counties_shapefile$urbanicity),
              layerId = counties,
              popup = paste0(counties_shapefile$NAMELSAD, ", ",counties_shapefile$state),
              fillOpacity = 0.5,
              weight = 1,
              color = urbanicity_color(counties_shapefile$urbanicity),
              opacity = 1,
              label = paste0(counties_shapefile$proportion_incarcerated, "%")
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Urbanicity of County",
    pal = urbanicity_color,
    values = ~urbanicity,
    opacity = 1
  )

###############################################################################
# scatterplot over capacity jails vs total number of jails per year
jail_capacity_2016 <- df %>%
  select(year, state, county_name, total_jail_pop, jail_rated_capacity, urbanicity) %>%
  filter(jail_rated_capacity > 0) %>%
  mutate(
    percent_full = round((total_jail_pop / jail_rated_capacity) * 100, 2)
  )

# For summary
number_in_overcapacity_jail <- jail_capacity_2016 %>%
  filter(percent_full > 100) %>%
  select(year, total_jail_pop) %>%
  group_by(year) %>%
  filter(year == 2016) %>%
  summarise(
    people_in_overcapacity_jail = round(sum(total_jail_pop))
  ) %>%
  pull(people_in_overcapacity_jail)

number_in_overcapacity_jail <- prettyNum(number_in_overcapacity_jail, big.mark=",", scientific = FALSE)
  
over_capacity_jails_2016 <- jail_capacity_2016 %>%
  filter(percent_full > 100) %>%
  select(year, percent_full, total_jail_pop) %>%
  group_by(year) %>%
  filter(year == 2016) %>%
  summarise(
    over_capacity_per_state = n()
  ) %>%
  pull(over_capacity_per_state)

# Total jails 
total_jails <- jail_capacity_2016 %>%
  select(year, percent_full) %>%
  group_by(year) %>%
  summarise(
    total_per_state = n()
  )
  
###############################################################################
# Capacity of ALL jails vs population, per year
jail_capacity_pop_year <- df %>%
  select(year, state, county_name, total_jail_pop, jail_rated_capacity) %>%
  filter(jail_rated_capacity > 0) %>%
  filter(year > 1980) %>%
  group_by(year) %>%
  summarise(
    yearly_jail_pop = sum(total_jail_pop, na.rm = TRUE),
    yearly_jail_capacity = sum(jail_rated_capacity, na.rm = TRUE)
  ) %>%
  select(year, yearly_jail_capacity, yearly_jail_pop) 

## Bar 
bars <- ggplot(data = jail_capacity_pop_year) +
  geom_col(mapping = aes(x = year, y = yearly_jail_capacity, fill = "Capacity")) +
  geom_col(mapping = aes(x = year, y = yearly_jail_pop, fill = "Population")) +
  labs(
    title = "Jail Capacity to Population 1981 - 2016",
    x = "Year",
    y = "# of People",
    fill = "Legend"
  )
