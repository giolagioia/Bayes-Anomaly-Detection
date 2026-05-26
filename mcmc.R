library(brms)

#Divide the data set for train and test 80% 20%
set.seed(123)
train_idx <- sample(seq_len(nrow(data)), 0.8 * nrow(data))

train <- data[train_idx, ]
test  <- data[-train_idx, ]

cat("\n--- TRAIN/TEST SPLIT ---\n")
cat("Training observations:", nrow(train), "\n")
cat("Test observations:", nrow(test), "\n")
cat("Training anomaly rate:", round(mean(train$Y), 6), "\n")
cat("Test anomaly rate:", round(mean(test$Y), 6), "\n")

# no more beta binomial, but brms
fit_bayes <- brm(
  Y ~ amount + balance + log_time + is_withdrawal + balance_amount_ratio + weekday,
  family = bernoulli(link = "logit"),
  data = train,
  prior = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  ),
  chains = 4,
  iter = 1200,
  warmup = 600,
  seed = 123
)

print(fit_bayes)

fixed_effects_summary <- as.data.frame(summary(fit_bayes)$fixed)
cat("\n--- MCMC DIAGNOSTICS ---\n")
cat("Post-warmup draws:", 4 * (1200 - 600), "\n")
cat("Maximum Rhat among fixed effects:",
    round(max(fixed_effects_summary$Rhat, na.rm = TRUE), 6), "\n")
cat("Minimum bulk ESS among fixed effects:",
    round(min(fixed_effects_summary$Bulk_ESS, na.rm = TRUE), 2), "\n")
cat("Minimum tail ESS among fixed effects:",
    round(min(fixed_effects_summary$Tail_ESS, na.rm = TRUE), 2), "\n")

#Frequentist approach
fit_freq <- glm(
  Y ~ amount + balance + log_time + is_withdrawal,
  family = binomial(link = "logit"),
  data = train
)

summary(fit_freq)
