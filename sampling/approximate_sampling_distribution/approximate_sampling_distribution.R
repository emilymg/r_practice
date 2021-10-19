# Approximate Sampling Distribution example
# code from Datacamp guided exercise in Course: Sampling in R

library(tidyverse)

# Sample rolling an 8-sided die 5 times, with replacement
five_rolls <- sample(
  1:8, size = 5, replace = TRUE
)

# Calculate the mean of five_rolls
mean(five_rolls)

# Replicate the sampling code 1000 times
sample_means_1000 <- replicate(
  n = 1000,
  expr = {
    five_rolls <- sample(1:8, size = 5, replace = TRUE)
    mean(five_rolls)
  }
)

# Wrap sample_means_1000 in the sample_mean column of a tibble
sample_means <- tibble(
  sample_mean = sample_means_1000
)

# Using sample_means, draw a bar plot of sample_mean as a factor
ggplot(sample_means, aes(x = factor(sample_mean))) +
  geom_bar()

# Conclusion: the approximate sampling distribution is similar but 
# not exactly the same as the exact sampling distribution