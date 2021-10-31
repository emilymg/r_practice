install.packages("assertive")
library(assertive)
library(tidyverse)
# Import data: San Francisco bike share ride data collected over a 30 day period.
# Contains start and end stations of each trip, the trip duration, and some user information.
bike <- readRDS("/media/em/ADATA SE770G/datasets/bike_share_rides_ch1_1.rds")

# Preview data structure
glimpse(bike)
range(bike$date)
# Remove text characters from duration column, assign as numeric
bike <- bike %>%
  mutate(duration_trimmed = str_remove(duration, "minutes")) %>%
  mutate(duration_mins = as.numeric(duration_trimmed))

# Assert duration_mins is numeric - no output if correct
assert_is_numeric(bike$duration_mins)

# Central Limit Theorem: For random and independent samples, the sampling distribution of a 
#   statistic becomes closer to the normal distribution as the number of trials increases.

# Set seed
set.seed(10)
# Sample 20 observations from duration_mins and take the mean
sample_mean_20 <- sample(bike$duration_mins, size = 20, replace = FALSE) %>%
  mean()
# Replicate sampling 20, 100 times, creating a vector of sample means
sample_means_100 <- replicate(100, sample(bike$duration_mins, size = 20, replace = FALSE) %>%
                            mean())
# Create a data frame of sample_means for plotting
samples_100 <- data.frame(mean = sample_means_100)

# Plot sample means as a histogram
ggplot(samples_100, aes(x = mean)) +
  geom_histogram(bins = 30) +
  scale_x_log10()

# Replicate again 10000 times
sample_means_10000 <- replicate(10000, sample(bike$duration_mins, size = 20, replace = FALSE) %>%
                            mean())
# Create a data frame of sample_means for plotting
samples_10000 <- data.frame(mean = sample_means_10000)

# Plot distribution of sample_means_10000
ggplot(samples_10000, aes(x = mean)) +
  geom_histogram(bins = 30) +
  scale_x_log10()

# Replicate again 100000 times
sample_means_100000 <- replicate(100000, sample(bike$duration_mins, size = 20, replace = FALSE) %>%
                                  mean())
# Create a data frame of sample_means for plotting
samples_100000 <- data.frame(mean = sample_means_100000)

# Plot distribution of sample_means_10000
ggplot(samples_100000, aes(x = mean)) +
  geom_histogram(bins = 30) +
  scale_x_log10()

# Calculate the total population mean of duration_mins
mean_pop <- mean(bike$duration_mins) 

# Create histogram of total population distribution of duration_mins
ggplot(bike, aes(duration_mins)) +
  geom_histogram(bins = 30) +
  geom_vline(aes(xintercept = mean_pop, color = "red")) +
  scale_x_log10() +
  theme(legend.position = "none")

# Regardless of sample size, the mean of the sampling distribution is close to the population mean.
# Mean of sample_means_100
mean(sample_means_100)
# Mean of sample_means_10000
mean(sample_means_10000)
# Mean of sample_means_100000
mean(sample_means_100000)

# As sample size increases, standard deviation decreases.
sd(sample_means_100)
sd(sample_means_10000)
sd(sample_means_100000)


