# Librarys  -------------------------------------------------------

library(gapminder)
library(dplyr)
library(ggplot2)

# Create first plot -------------------------------------------------------

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a scatter plot with gdpPercap on the x-axis and lifeExp on the y-axis
gapminder_1952 %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) + 
  geom_point()


# Change to put pop on the x-axis and gdpPercap on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()


# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
gapminder_1952 %>% 
  ggplot(aes(x = pop, y = lifeExp)) + 
  geom_point()

# Using LOG-SCALE
# Create a scatter plot with gdpPercap on the x-axis and lifeExp on the y-axis
gapminder_1952 %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp)) + 
  geom_point() +
  scale_x_log10()


# Using COLOR
# Scatter plot comparing pop and lifeExp, with color representing continent
gapminder_1952 %>% 
  ggplot(aes(x = pop, y = lifeExp, color = continent)) + 
  geom_point() +
  scale_x_log10()

# ADD SIZE
# Add the size aesthetic to represent a country's gdpPercap
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent, size = gdpPercap)) +
  geom_point() +
  scale_x_log10()

# Faceting
# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~continent)


# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~year)



# Grouping and Summarizing  -------------------------------------------

gapminder %>% 
  summarise(median = median(lifeExp))


# Filter for 1957 then summarize the median life expectancy
gapminder %>% 
  filter(year == 1957) %>% 
  summarise(medianLifeExp = median(lifeExp))

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder %>% 
  filter(year == 1957) %>% 
  summarise(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))



# <!-- Groupby___ -->

# Find median life expectancy and maximum GDP per capita in each year
gapminder %>% 
  group_by(year) %>% 
  summarise(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))


# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>% 
  filter(year == 1957) %>% 
  group_by(continent) %>% 
  summarise(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))


# Find median life expectancy and maximum GDP per capita in each continent/year combination
gapminder %>% 
  #filter(year == 1957) %>% 
  group_by(year, continent) %>% 
  summarise(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))



# # <!-- Visualization and group_by  -->

# <!-- -->

by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time

by_year %>% 
  ggplot(aes(x = year, y = medianLifeExp)) +
  geom_point() + 
  expand_limits(y = 0)

# <!-- -->

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
by_year_continent %>% 
  ggplot(aes(x = year, y = medianGdpPercap, color = continent))+
  geom_point()+
  expand_limits(y = 0)

# <!-- -->

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>%
  filter(year==2007) %>% 
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap),
            medianLifeExp = median(lifeExp))

# Use a scatter plot to compare the median GDP and median life expectancy
by_continent_2007 %>% 
  ggplot(aes(x = medianGdpPercap, y= medianLifeExp, color = continent)) +
  geom_point()


# Typer and Visualizations ------------------------------------------------

# <!-- -->

# Summarize the median gdpPercap by year, then save it as by_year
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
by_year %>% 
  ggplot(aes(x = year, y=medianGdpPercap)) +
  geom_line() +
  expand_limits(y = 0)

# <!-- -->

# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
by_year_continent %>% 
  ggplot(aes(x = year, y=medianGdpPercap, color = continent)) +
  geom_line() +
  expand_limits(y = 0)

# <!-- GEOM BAR / GEOM COL -->

# Summarize the median gdpPercap by continent in 1952
by_continent <- gapminder %>%
  filter(year==1952) %>% 
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
by_continent %>% 
  ggplot(aes(x = continent, y =medianGdpPercap)) +
  geom_col()

#---

# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>%
  filter(continent == 'Oceania', year==1952) 

# Create a bar plot of gdpPercap by country
oceania_1952 %>% 
  ggplot(aes(x = country, y =gdpPercap)) +
  geom_col()

# <!-- HIST -->

gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

# Create a histogram of population (pop_by_mil)
gapminder_1952 %>% 
  ggplot(aes(x = pop_by_mil)) +
  geom_histogram(bins = 50)


gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop), with x on a log scale
gapminder_1952 %>% 
  ggplot(aes(x = pop)) +
  geom_histogram(bins = 50) + 
  scale_x_log10()


# <!-- BOXPLOT -->

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a boxplot comparing gdpPercap among continents

gapminder_1952 %>% 
  ggplot(aes(x = continent, y = gdpPercap)) +
  geom_boxplot() + 
  scale_y_log10() + 
  ggtitle('Comparing GDP per capita across continents')

# Vamos procurar quem nas Américas são os Outliers - curiosidade
gapminder %>% 
  filter(year == 1952, continent == 'Americas') %>% 
  arrange(desc(gdpPercap))

gapminder %>% 
  filter(year == 2007, continent == 'Americas') %>% 
  arrange(desc(gdpPercap))
  
