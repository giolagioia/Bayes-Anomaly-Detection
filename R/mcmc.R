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

bayes_formula <- Y ~ amount + balance + log_time + is_withdrawal +
  balance_amount_ratio + weekday

bayes_priors <- list(
  normal_0_1 = c(
    prior(normal(0, 1), class = "b"),
    prior(normal(0, 1), class = "Intercept")
  ),
  normal_0_2 = c(
    prior(normal(0, 2), class = "b"),
    prior(normal(0, 2), class = "Intercept")
  ),
  cauchy_0_2_5 = c(
    prior(cauchy(0, 2.5), class = "b"),
    prior(cauchy(0, 2.5), class = "Intercept")
  )
)

bayes_prior_labels <- c(
  normal_0_1 = "Normal(0,1)",
  normal_0_2 = "Normal(0,2)",
  cauchy_0_2_5 = "Cauchy(0,2.5)"
)

fit_one_bayes_model <- function(prior_name) {
  cat("\n--- FITTING BAYESIAN MODEL:", bayes_prior_labels[[prior_name]], "---\n")
  brm(
    bayes_formula,
    family = bernoulli(link = "logit"),
    data = train,
    prior = bayes_priors[[prior_name]],
    chains = 4,
    iter = 1200,
    warmup = 600,
    seed = 123
  )
}

fit_bayes_models <- lapply(names(bayes_priors), fit_one_bayes_model)
names(fit_bayes_models) <- names(bayes_priors)

# Keep the Cauchy(0,2.5) fit as the main Bayesian model for plots and threshold tuning.
fit_bayes <- fit_bayes_models$cauchy_0_2_5

cat("\n--- BAYESIAN MODEL SUMMARIES ---\n")
for (prior_name in names(fit_bayes_models)) {
  cat("\nPrior:", bayes_prior_labels[[prior_name]], "\n")
  print(fit_bayes_models[[prior_name]])
}

mcmc_diagnostics <- do.call(rbind, lapply(names(fit_bayes_models), function(prior_name) {
  fixed_effects_summary <- as.data.frame(summary(fit_bayes_models[[prior_name]])$fixed)
  data.frame(
    prior = bayes_prior_labels[[prior_name]],
    post_warmup_draws = 4 * (1200 - 600),
    max_rhat = max(fixed_effects_summary$Rhat, na.rm = TRUE),
    min_bulk_ess = min(fixed_effects_summary$Bulk_ESS, na.rm = TRUE),
    min_tail_ess = min(fixed_effects_summary$Tail_ESS, na.rm = TRUE)
  )
}))

cat("\n--- MCMC DIAGNOSTICS ---\n")
print(mcmc_diagnostics, row.names = FALSE, digits = 4)

#Frequentist approach
fit_freq <- glm(
  Y ~ amount + balance + log_time + is_withdrawal,
  family = binomial(link = "logit"),
  data = train
)

summary(fit_freq)
