# The Binomial Distribution

# rbinom() generates a vector of binomial distributed random deviates given
#   vector length(n), number of trials(size), and a fixed probability.
# Args: rbinom(n, size, p) 
# Coin flip: where (n = # of trials, size = # of coins, p = probability of success(heads))
# where heads = 1, tails = 0

# 1 flip of 1 coin
rbinom(1 ,1, 0.5)

# 1 flip of 8 coins - how many times will heads be the outcome?
rbinom(1, 8, 0.5)

#8 flips of 1 coin - output is result of each coin flip
rbinom(8, 1, 0.5)

# dbinom() finds the probability of getting a certain number of successes(x) in a certain number of 
#   trials(size, n) where the probability of success on each trial is fixed(prob, p):
#   Probability Density Function of the binomial distribution.
# Args: dbinom(x, size, prob, log = FALSE)
# Probability of getting heads 7 times out of 10: P(heads = 7)
dbinom(7, 10, 0.5)

# pbinom() returns the area to the left (<=) of a random variable q in the binomial distribution:
#   Cumulative Density Function of the binomial distribution.
# Args: pbinom(q(vector of quantiles), size, prob, lower.tail = TRUE, log.p = FALSE)
# Probability of getting heads 7 or less times out of 10: P(heads <= 7)
pbinom(7, 10, 0.5)

# Area to the right (>): set lower.tail to TRUE
# Probability of getting heads more than 7 times: P(heads > 7)
pbinom(7, 10, 0.5, lower.tail = FALSE)
# Same as:
1 - (pbinom(7, 10, 0.5))

# Visualizing random numbers - Beta distribution

randoms <- data.frame(
  beta = rbeta(5000, shape1 = 2, shape2 = 2)
)

ggplot(randoms, aes(beta)) +
  geom_histogram(binwidth = 0.1)

# Pseudo-random number generation using rnorm()
# Starting from a particular seed value, all future random numbers will be the same.
set.seed(20000229) 
# Args: rnorm(n, mean = 0, sd = 1)
rnorm(5) 

# Uniform vs Normal Distribution
n_numbers <- 5000

randoms_u_vs_n <- data.frame(
  uniform = runif(n_numbers, min = -3, max = 3),
  normal = rnorm(n_numbers, mean = 5, sd = 2)
)

ggplot(randoms_u_vs_n, aes(uniform)) +
  geom_histogram(binwidth = 0.5)
ggplot(randoms_u_vs_n, aes(normal)) +
  geom_histogram(binwidth = 0.5)
