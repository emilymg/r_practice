library(tidyverse)
#load gapminder data
library(gapminder)

#examine data structure
glimpse(gapminder)

#what years are included as observations?
levels(factor(gapminder$year))

#filter the data for year, arrange by gdpPercap descending
gap_2007 <- gapminder %>%
  filter(year == 2007) %>%
  arrange(desc(gdpPercap))

#plot life expectancy described by gdp per capita, facet by continent
#color by continent, size = population
ggplot(gap_2007, aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point() +
  scale_x_log10() +
  facet_grid(~continent) + 
  labs(title = "Life Expectancy vs GDP per Capita, 2007",
       x = "GDP per Capita",
       y = "Life Expectancy")
