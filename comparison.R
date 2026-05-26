library(pROC)
library(ggplot2)

# FREQUENTIST MODEL
p_freq <- predict(fit_freq, newdata = test, type = "response")
roc_freq <- roc(test$Y, p_freq)
auc_freq <- auc(roc_freq)

# BAYESIAN MODEL
p_draws <- posterior_epred(fit_bayes, newdata = test)
p_bayes <- colMeans(p_draws)

roc_bayes <- roc(test$Y, p_bayes)
auc_bayes <- auc(roc_bayes)

# ACCURACY
acc_freq <- mean((p_freq > 0.5) == test$Y)
acc_bayes <- mean((p_bayes > 0.5) == test$Y)

# LOG LOSS
logloss <- function(y, p) {
  -mean(y * log(p + 1e-10) + (1 - y) * log(1 - p + 1e-10))
}

logloss_freq <- logloss(test$Y, p_freq)
logloss_bayes <- logloss(test$Y, p_bayes)

# Optimal threshold from ROC curve
opt_threshold <- coords(roc_bayes, "best", best.method = "youden")$threshold

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

pred_freq_05 <- as.integer(p_freq > 0.5)
pred_bayes_05 <- as.integer(p_bayes > 0.5)
pred_bayes_opt <- as.integer(p_bayes > opt_threshold)

performance_metrics <- data.frame(
  model = c("Frequentist logistic", "Bayesian logistic", "Bayesian logistic optimal threshold"),
  threshold = c(0.5, 0.5, as.numeric(opt_threshold)),
  auc = c(as.numeric(auc_freq), as.numeric(auc_bayes), as.numeric(auc_bayes)),
  accuracy = c(acc_freq, acc_bayes, mean(pred_bayes_opt == test$Y)),
  logloss = c(logloss_freq, logloss_bayes, logloss_bayes),
  rbind(
    metric_set(test$Y, pred_freq_05),
    metric_set(test$Y, pred_bayes_05),
    metric_set(test$Y, pred_bayes_opt)
  )
)

cat("\n--- PREDICTIVE PERFORMANCE ON TEST SET ---\n")
print(performance_metrics, row.names = FALSE, digits = 4)

#ROC Curve
png(file.path("plots", "04_roc_comparison.png"),
    width = 1200, height = 800, res = 150)
plot(roc_freq, col = "red", main = "ROC Comparison")
lines(roc_bayes, col = "blue")
legend("bottomright",
       legend = c("Frequentist", "Bayesian"),
       col = c("red", "blue"),
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
    title = "Bayesian predictive probabilities with 95% credible intervals",
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
