Y_seq <- data$Y
alpha0 <- 1
beta0  <- 1

n_seq <- length(Y_seq) # number of known samples

#inizialization
post_mean <- numeric(n_seq)
post_low  <- numeric(n_seq)
post_high <- numeric(n_seq)

for (t in 1:n_seq) {
  k_t <- sum(Y_seq[1:t]) # progressive sum of k in the dataset
  
  alpha_t <- alpha0 + k_t
  beta_t  <- beta0 + t - k_t
  
  post_mean[t] <- alpha_t / (alpha_t + beta_t) #expected value definition
  
  ci <- qbeta(c(0.05, 0.95), alpha_t, beta_t)
  post_low[t] <- ci[1]
  post_high[t] <- ci[2]
}

cat("\n--- SEQUENTIAL BAYESIAN LEARNING ---\n")
cat("Final posterior mean:", round(tail(post_mean, 1), 6), "\n")
cat("Final 90% credible interval:",
    paste0("[", round(tail(post_low, 1), 6), ", ",
           round(tail(post_high, 1), 6), "]"), "\n")
cat("Posterior mean min / max:",
    round(min(post_mean), 6), "/",
    round(max(post_mean), 6), "\n")

png(file.path("plots", "03_sequential_bayesian_learning.png"),
    width = 1200, height = 800, res = 150)
plot(post_mean, type="l", col="blue",
     ylim=c(0, 1),
     xlab="transactions",
     ylab="P(anomaly)",
     main="Bayesian expected value over time")
lines(post_low, col = "blue", lty = 2)
lines(post_high, col = "blue", lty = 2)
legend("topright",
       legend = c("Posterior mean", "90% credible interval"),
       col = "blue",
       lty = c(1, 2),
       bty = "n")
dev.off()
