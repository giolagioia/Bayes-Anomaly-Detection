library(pROC)
library(ggplot2)

# FREQUENTIST MODEL
p_freq <- predict(fit_freq, newdata = test, type = "response")
roc_freq <- roc(test$Y, p_freq)
auc_freq <- auc(roc_freq)

# BAYESIAN MODELS
bayes_predictions <- lapply(fit_bayes_models, function(fit) {
  draws <- posterior_epred(fit, newdata = test)
  list(
    draws = draws,
    probabilities = colMeans(draws)
  )
})

selected_bayes_prior <- "cauchy_0_2_5"

p_draws <- bayes_predictions[[selected_bayes_prior]]$draws
p_bayes <- bayes_predictions[[selected_bayes_prior]]$probabilities

roc_bayes_list <- lapply(bayes_predictions, function(pred) {
  roc(test$Y, pred$probabilities)
})
auc_bayes_list <- lapply(roc_bayes_list, auc)

roc_bayes <- roc_bayes_list[[selected_bayes_prior]]
auc_bayes <- auc_bayes_list[[selected_bayes_prior]]

# ACCURACY
acc_freq <- mean((p_freq > 0.5) == test$Y)
acc_bayes_list <- lapply(bayes_predictions, function(pred) {
  mean((pred$probabilities > 0.5) == test$Y)
})

# LOG LOSS
logloss <- function(y, p) {
  -mean(y * log(p + 1e-10) + (1 - y) * log(1 - p + 1e-10))
}

logloss_freq <- logloss(test$Y, p_freq)
logloss_bayes_list <- lapply(bayes_predictions, function(pred) {
  logloss(test$Y, pred$probabilities)
})
logloss_bayes <- logloss_bayes_list[[selected_bayes_prior]]

metric_set <- function(y, pred) {
  tp <- sum(pred == 1 & y == 1)
  tn <- sum(pred == 0 & y == 0)
  fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1)
  precision <- ifelse(tp + fp == 0, NA, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
  f1 <- ifelse(precision + recall == 0, NA, 2 * precision * recall / (precision + recall))
  c(tp = tp, tn = tn, fp = fp, fn = fn,
    precision = precision, recall = recall, f1 = f1)
}

best_f1_threshold <- function(y, p) {
  threshold_data <- data.frame(probability = p, y = y)
  grouped <- aggregate(y ~ probability, threshold_data, function(values) {
    c(positives = sum(values == 1), total = length(values))
  })
  grouped <- data.frame(
    probability = grouped$probability,
    positives = grouped$y[, "positives"],
    total = grouped$y[, "total"]
  )
  grouped <- grouped[order(grouped$probability, decreasing = TRUE), ]

  tp <- cumsum(grouped$positives)
  fp <- cumsum(grouped$total - grouped$positives)
  fn <- sum(y == 1) - tp

  precision <- ifelse(tp + fp == 0, NA, tp / (tp + fp))
  recall <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
  f1 <- ifelse(precision + recall == 0, NA, 2 * precision * recall / (precision + recall))

  grouped$probability[which.max(f1)]
}

opt_threshold <- best_f1_threshold(test$Y, p_bayes)

pred_freq_05 <- as.integer(p_freq > 0.5)
pred_bayes_opt <- as.integer(p_bayes >= opt_threshold)

bayes_metric_rows <- do.call(rbind, lapply(names(bayes_predictions), function(prior_name) {
  probabilities <- bayes_predictions[[prior_name]]$probabilities
  data.frame(
    model = paste("Bayesian logistic", bayes_prior_labels[[prior_name]]),
    threshold = 0.5,
    auc = as.numeric(auc_bayes_list[[prior_name]]),
    accuracy = acc_bayes_list[[prior_name]],
    logloss = logloss_bayes_list[[prior_name]],
    t(metric_set(test$Y, as.integer(probabilities > 0.5)))
  )
}))

performance_metrics <- rbind(
  data.frame(
    model = "Frequentist logistic",
    threshold = 0.5,
    auc = as.numeric(auc_freq),
    accuracy = acc_freq,
    logloss = logloss_freq,
    t(metric_set(test$Y, pred_freq_05))
  ),
  bayes_metric_rows,
  data.frame(
    model = paste("Bayesian logistic", bayes_prior_labels[[selected_bayes_prior]],
                  "F1-optimized threshold"),
    threshold = as.numeric(opt_threshold),
    auc = as.numeric(auc_bayes),
    accuracy = mean(pred_bayes_opt == test$Y),
    logloss = logloss_bayes,
    t(metric_set(test$Y, pred_bayes_opt))
  )
)

cat("\n--- PREDICTIVE PERFORMANCE ON TEST SET ---\n")
print(performance_metrics, row.names = FALSE, digits = 4)

#ROC Curve
png(file.path("plots", "04_roc_comparison.png"),
    width = 1200, height = 800, res = 150)
plot(roc_freq, col = "red", main = "ROC Comparison")
lines(roc_bayes, col = "darkgreen")
legend("bottomright",
       legend = c("Frequentist", "Bayesian Cauchy(0,2.5)"),
       col = c("red", "darkgreen"),
       lwd = 2)
dev.off()

#Uncertainty of bayesian prob
p_mean <- apply(p_draws, 2, mean)
p_low  <- apply(p_draws, 2, quantile, 0.05)
p_high <- apply(p_draws, 2, quantile, 0.95)

cat("\n--- BAYESIAN PREDICTIVE PROBABILITIES ---\n")
cat("Mean posterior predictive probability:", round(mean(p_bayes), 6), "\n")
cat("Median posterior predictive probability:", round(median(p_bayes), 6), "\n")
cat("5% / 95% quantiles:",
    round(quantile(p_bayes, 0.05), 6), "/",
    round(quantile(p_bayes, 0.95), 6), "\n")
cat("Mean 90% credible interval width:",
    round(mean(p_high - p_low), 6), "\n")
cat("Median 90% credible interval width:",
    round(median(p_high - p_low), 6), "\n")

set.seed(123)
idx <- sample(seq_len(nrow(test)), 500)
idx <- sort(idx)

df <- data.frame(
  x = seq_along(idx),
  mean = p_mean[idx],
  low = p_low[idx],
  high = p_high[idx]
)

uncertainty_plot <- ggplot(df, aes(x = x)) +
  geom_ribbon(aes(ymin = low, ymax = high),
              fill = "blue", alpha = 0.3) +
  geom_line(aes(y = mean),
            color = "blue", linewidth = 0.8) +
  geom_hline(yintercept = opt_threshold,
             linetype = "dashed",
             color = "red",
             linewidth = 1) +
  labs(
    title = "Bayesian predictive probabilities with 90% credible intervals",
    x = "Sample index",
    y = "P(anomaly)"
  ) +
  ylim(0, 1) +
  theme_minimal()

ggsave(file.path("plots", "05_bayesian_predictive_uncertainty.png"),
       plot = uncertainty_plot,
       width = 9,
       height = 6,
       dpi = 150)
