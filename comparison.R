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

# PRINT RESULTS
auc_freq
auc_bayes
acc_freq
acc_bayes
logloss_freq
logloss_bayes

#ROC Curve
plot(roc_freq, col = "red", main = "ROC Comparison")
lines(roc_bayes, col = "blue")
legend("bottomright",
       legend = c("Frequentist", "Bayesian"),
       col = c("red", "blue"),
       lwd = 2)

#Uncertainty of bayesian prob
p_mean <- apply(p_draws, 2, mean)
p_low  <- apply(p_draws, 2, quantile, 0.05)
p_high <- apply(p_draws, 2, quantile, 0.95)

set.seed(123)
idx <- sample(seq_len(nrow(test)), 500)
idx <- sort(idx)

df <- data.frame(
  x = seq_along(idx),
  mean = p_mean[idx],
  low = p_low[idx],
  high = p_high[idx]
)

ggplot(df, aes(x = x)) +
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

