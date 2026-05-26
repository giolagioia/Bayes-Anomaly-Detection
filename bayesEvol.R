Y_seq <- data$Y
alpha0 <- 1
beta0  <- 1

n <- length(Y_seq) # number of known samples

#inizialization
post_mean <- numeric(n)
post_low  <- numeric(n)
post_high <- numeric(n)

for (t in 1:n) {
  k_t <- sum(Y_seq[1:t]) # progressive sum of k in the dataset
  
  alpha_t <- alpha0 + k_t
  beta_t  <- beta0 + t - k_t
  
  post_mean[t] <- alpha_t / (alpha_t + beta_t) #expected value definition
  
  ci <- qbeta(c(0.05, 0.95), alpha_t, beta_t)
  post_low[t] <- ci[1]
  post_high[t] <- ci[2]
}

plot(post_mean, type="l", col="blue",
     ylim=c(0, 1),
     xlab="transactions",
     ylab="P(anomaly)",
     main="Bayesian expected value over time")

