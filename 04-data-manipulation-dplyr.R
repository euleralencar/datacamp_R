library(ggplot2)
library(tidyverse)

link = 'https://assets.datacamp.com/production/repositories/4984/datasets/a924bf7063f02a5445e1f49cc1c75c78e018ac4c/counties.rds'

counties <- readRDS(url(link,"rb"))


# ---

# Selected data
counties_selected <- counties %>%
  select(region, state, county, metro, population, walk)

# Use count to find the number of counties in each region
counties_selected %>%
  count(region, sort = TRUE)

# --

# Selected data
counties_selected <- counties %>%
  select(county, region, state, population, citizens)

# Find number of counties per state, weighted by citizens, sorted in descending order
counties_selected %>%
  count(state, wt  = citizens, sort = TRUE)

# --

counties_selected <- counties %>%
  select(county, region, state, population, walk)


counties_selected %>%
  # Add population_walk containing the total number of people who walk to work 
  mutate(population_walk = (walk/100)*population) %>% 
  # Count weighted by the new column, sort in descending order
  count(state, wt = population_walk, sort = TRUE)

# --

# Selected data
counties_selected <- counties %>%
  select(county, region, state, population, citizens)

# Find number of counties per state, weighted by citizens, sorted in descending order
counties_selected %>%
  count(state, wt  = citizens)

# Esse comando é o mesmo que o SUM. Veja:

counties_selected %>% 
  group_by(state) %>% 
  summarise(n = sum(citizens))

# Find the greatest number of citizens who walk to work
counties_selected %>%
  group_by(region) %>%
  top_n(1, walk)

# --- 

# Selected data
counties_selected <- counties %>%
  select(region, state, county, population, income)

# Finding the highest-income state in each region
counties_selected %>%
  group_by(region, state) %>%
  # Calculate average income
  summarise(average_income = mean(income)) %>% 
  # Find the highest income state in each region
  top_n(1, average_income)

# ---


# Selected data
counties_selected <- counties %>%
  select(state, metro, population)

#  In how many states do more people live in metro areas than non-metro areas?
counties_selected %>%
  # Find the total population for each combination of state and metro
  group_by(state, metro) %>% 
  summarise(total_pop = sum(population)) %>% 
  # Extract the most populated row for each state
  top_n(1, total_pop) %>% 
  # Count the states with more people in Metro or Nonmetro areas
  ungroup() %>% 
  count(metro)


# Selection adv -----------------------------------------------------------

# select(column1:column10)
# select(state, country, contains('work')) 
# select(state, country, starts_with('income')) 
# other possibilities -> ends_with, last_col
# select(-state) -> remove
# I can use select to rename the column


# Simple count - Renaming a column after count
counties %>%
  # Count the number of counties in each state
  count(state) %>%
  # Rename the n column to num_counties
  rename(num_counties = n)

# Renaming a column as part of a select
counties %>%
  # Select state, county, and poverty as poverty_rate
  select(state, county, poverty_rate = poverty)



# Glimpse the counties table
glimpse(counties)

counties %>%
  # Select state, county, population, and industry-related columns
  select(state, county, population, professional:production) %>% 
  # Arrange service in descending order 
  arrange(desc(service))


counties %>%
  # Select the state, county, population, and those ending with "work"
  select(state, county, population, ends_with('work')) %>% 
  # Filter for counties that have at least 50% of people engaged in public work
  filter(public_work > 50)

# trasmute() = Select + Mutate

counties %>%
  # Keep the state, county, and populations columns, and add a density column
  transmute(state, county, population, density = population/land_area) %>% 
  # Filter for counties with a population greater than one million 
  filter(population > 1000*1000) %>% 
  # Sort density in ascending order 
  arrange(density)

# All together

# Change the name of the unemployment column
counties %>%
  rename(unemployment_rate = unemployment) %>% 

  # Keep the state and county columns, and the columns containing poverty
counties %>%
  select(state, county, contains("poverty"))

# Calculate the fraction_women column without dropping the other columns
counties %>%
  mutate(fraction_women = women / population)

# Keep only the state, county, and employment_rate columns
counties %>%
  transmute(state, county, employment_rate = employed / population)


# Case Study: The babynames Dataset ---------------------------------------

library(ggplot2)
library(tidyverse)

link = 'https://assets.datacamp.com/production/repositories/4984/datasets/a924ac5d86adba2e934d489cb9db446236f62b2c/babynames.rds'

babynames <- readRDS(url(link,"rb"))

# year        name        number

# Find the names in 1990 and order
babynames %>%
  # Filter for the year 1990
  filter(year == 1990) %>% 
  # Sort the number column in descending order 
  arrange(desc(number))

# --

# Find the most common name in each year
babynames %>%
  group_by(year) %>% 
  top_n(1, number)

# --

selected_names <- babynames %>%
  # Filter for the names Steven, Thomas, and Matthew 
  filter(name %in% c('Steven', 'Thomas', 'Matthew'))

# Plot the names using a different color for each name
ggplot(selected_names, aes(x = year, y = number, color = name)) +
  geom_line()


# Grouped mutates

# Calculate the fraction of people born each year with the same name
babynames %>%
  group_by(year) %>% 
  mutate(year_total = sum(number)) %>% 
  ungroup() %>% 
  mutate(fraction = number / year_total) %>% 
  # Find the year each name is most common
  group_by(name) %>% 
  top_n(1, fraction)


# Adding the total and maximum for each name
babynames %>%
  # Add columns name_total and name_max for each name
  group_by(name) %>% 
  mutate(name_total = sum(number),
         name_max = max(number))

# E se eu quiser saber em qual ano aquele valor foi o máximo. Consigo fazer
# pelo Mutate

# Continuação do anterior -->
names_normalized  <- babynames %>%
  # Add columns name_total and name_max for each name
  group_by(name) %>%
  mutate(name_total = sum(number),
         name_max = max(number)) %>%
  # Ungroup the table 
  ungroup() %>% 
  # Add the fraction_max column containing the number by the name maximum
  mutate(fraction_max = round(number/ name_max, 4)*100)


# Visualizing the normalized change in popularity
names_filtered <- names_normalized %>%
  # Filter for the names Steven, Thomas, and Matthew
  filter(name %in% c('Steven', 'Thomas', 'Matthew'))
# Visualize these names over time
names_filtered %>%
  ggplot(aes(x = year, y = fraction_max, color = name)) +
  geom_line()


# Window function

v <- c(1, 3, 6, 14)
v1 <- lag(v)
v
v1

# We can use in dataframe to see jumps between years

babynames %>%
  group_by(year) %>% 
  mutate(year_total = sum(number)) %>% 
  ungroup() %>% 
  mutate(fraction = number / year_total) %>% 
  filter(name == 'Matthew') %>% 
  mutate(lag_fraction = lag(fraction),
         difference = fraction - lag(fraction)) %>%
  arrange(desc(difference))

  
# Vamos
babynames_fraction <- 
babynames %>%
  group_by(year) %>% 
  mutate(year_total = sum(number)) %>% 
  ungroup() %>% 
  mutate(fraction = number / year_total) 

babynames_fraction %>% 
  filter(name == 'Matthew') %>% 
  ggplot(aes(x= year, y =fraction)) + 
  geom_line()

# Curiosidade ----
nome = 'Flavia'
babynames %>%
  group_by(year) %>% 
  mutate(year_total = sum(number)) %>% 
  ungroup() %>% 
  mutate(fraction = number / year_total) %>% 
  filter(name == nome) %>% 
  ggplot(aes(x= year, y =number)) + 
  geom_line() + 
  ggtitle(nome)

# ---

babynames_fraction %>%
  # Arrange the data in order of name, then year 
  arrange(name, year) %>% 
  # Group the data by name
  group_by(name) %>% 
  # Add a ratio column that contains the ratio of fraction between each year
  mutate(ratio = fraction/lag(fraction))


# --

# babynames_ratios_filtered
babynames_ratios_filtered <- babynames_fraction %>%
  arrange(name, year) %>%
  group_by(name) %>%
  mutate(ratio = fraction / lag(fraction)) %>%
  filter(fraction >= 0.00001)

babynames_ratios_filtered %>%
  # Extract the largest ratio from each name 
  top_n(1, ratio) %>% 
  # Sort the ratio column in descending order 
  arrange(desc(ratio)) %>% 
  # Filter for fractions greater than or equal to 0.001
  filter(fraction >= 0.001)
