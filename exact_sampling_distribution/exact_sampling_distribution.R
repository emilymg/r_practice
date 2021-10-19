# Exact Sampling Distribution example

library(tidyverse)

# Expand a grid representing 5 8-sided dice
dice <- expand_grid(
  die1 = 1:8,
  die2 = 1:8,
  die3 = 1:8,
  die4 = 1:8,
  die5 = 1:8,
) %>%  
  # Add a column of mean rolls
  mutate(mean_roll = (die1 + die2 + die3 + die4 + die5) / 5)

# Using dice, draw a bar plot of mean_roll as a factor
ggplot(dice, aes(x = factor(mean_roll))) +
  geom_bar()
