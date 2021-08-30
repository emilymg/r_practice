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

# Save a new table, filter and arrange data as needed
gap_2007 <- gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(pop))

# Plot lifeExp described by gdpPercap
# Color and facet by continent, set size to population
ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_wrap(~continent)
