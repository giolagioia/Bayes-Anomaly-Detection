# =========================
# MODEL COMPARISON
# Frequentist vs Bayesian
# =========================

library(pROC)

#Frequentist prediction
p_freq <- predict(
  fit_freq,
  newdata = test,
  type = "response"
)

#Bayesian predictions
p_draws <- posterior_epred(
  fit_bayes,
  newdata = test
)

# posterior predictive mean
p_bayes <- colMeans(p_draws)

# credible intervals
p_ci <- t(apply(
  p_draws,
  2,
  quantile,
  probs = c(0.025, 0.975)
))

colnames(p_ci) <- c("low", "high")

# uncertainty width
uncertainty <- p_ci[,2] - p_ci[,1]

# -------------------------
# 3. ROC and AUC
# -------------------------
roc_freq <- roc(test$Y, p_freq)
roc_bayes <- roc(test$Y, p_bayes)

auc_freq <- auc(roc_freq)
auc_bayes <- auc(roc_bayes)

print(auc_freq)
print(auc_bayes)

# -------------------------
# 5. Bayesian uncertainty plot
# -------------------------
ord <- order(p_bayes)

plot(
  p_bayes[ord],
  type = "l",
  col = "red",
  lwd = 2,
  ylim = c(0,1),
  xlab = "Transactions sorted by risk",
  ylab = "Predicted fraud probability",
  main = "Bayesian predictive uncertainty"
)

lines(
  p_ci[ord,1],
  col = "gray",
  lty = 2
)

lines(
  p_ci[ord,2],
  col = "gray",
  lty = 2
)

# -------------------------
# 6. Compare predictions
# -------------------------
plot(
  p_freq,
  p_bayes,
  pch = 16,
  cex = 0.5,
  xlab = "Frequentist probabilities",
  ylab = "Bayesian probabilities",
  main = "Prediction comparison"
)

abline(0,1,col="red",lwd=2)

# -------------------------
# 7. Most uncertain transactions
# -------------------------
most_uncertain <- order(
  uncertainty,
  decreasing = TRUE
)[1:10]

test[most_uncertain, ]

# -------------------------
# 8. Uncertainty summary
# -------------------------
summary(uncertainty)

