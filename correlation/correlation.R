# Show relationship between bodywt and awake time
p_sleep_bodywt <- msleep %>%
  mutate(log_bodywt = log(bodywt)) %>%
  ggplot(aes(log_bodywt, awake)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE)

# Calculate correlation
msleep$log_bodywt <- log(msleep$bodywt)
cor(msleep$log_bodywt, msleep$awake)
