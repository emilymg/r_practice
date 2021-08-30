library(tidyverse)

# Load gapminder data
library(gapminder)

# Examine the data
glimpse(gapminder)

# What years are in the data?
levels(factor(gapminder$year))

# Add a new column
gapminder %>%
  mutate(lifeExpMonths = lifeExp * 12)

# Use summarize to perform aggregations
gapminder %>%
  group_by(continent) %>%
  summarize(total_pop = sum(pop))

# Plotting summarized data, change in median life expectancy over time
gapminder %>%
  group_by(year, continent) %>%
  summarize(medianLifeExp = median(lifeExp)) %>%
  ggplot(aes(x = year, y = medianLifeExp, color = continent)) +
    geom_line() +
    expand_limits(y = 0) +
  ggtitle("Change in Median Life Expectancy over Time")

# Save a new table, filter and arrange data as needed
gap_2007 <- gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(pop))

# Plot lifeExp described by gdpPercap
# Color and facet by continent, set size to population
ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~continent) +
  ggtitle("Life Expectancy Described by GDP per Capita")

# Look at median GDP per continent in a specific year, by continent
gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap)) %>%
  ggplot(aes(x = continent, y = medianGdpPercap)) +
    geom_col() +
    ggtitle("Median GDP per Capita in 2007")

# Look at the distribution of world populations
gapminder %>%
  filter(year == 1952) %>%
  group_by(country) %>%
  mutate(pop_millions = pop / 1000000) %>%
  ggplot(aes(x = pop_millions)) +
    geom_histogram(bins = 50) +
    scale_x_log10() +
  ggtitle("Country Populations (millions) in 1952")

# Look at the distribution of GDP across continents in a particular year
gapminder %>%
  filter(year == 1972) %>%
  ggplot(aes(x = continent, y = gdpPercap)) +
    geom_boxplot() +
    scale_y_log10() +
    ggtitle("Comparing GDP of all Continents in 1972")