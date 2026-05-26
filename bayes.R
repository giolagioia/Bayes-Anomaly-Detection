prior1 <- c(alpha = 1, beta = 1) # non informative prior distribution
prior2 <- c(alpha = 1, beta = 19) #information based on number of anomalies
prior3 <- c(alpha = 5, beta = 95) #even stronger prior always based on 5%

#Considering a subsample of data, where n,k << alpha, beta
set.seed(49)
Y_train <- sample(data$Y, 250)

n <- length(Y_train) # number of rows in subset data
k <- sum(Y_train) # number of successes in subset data

posterior1 <- c(
  alpha = prior1[["alpha"]] + k,
  beta  = prior1[["beta"]] + n - k
)

posterior2 <- c(
  alpha = prior2[["alpha"]] + k,
  beta  = prior2[["beta"]] + n - k
)

posterior3 <- c(
  alpha = prior3[["alpha"]] + k,
  beta  = prior3[["beta"]] + n - k
)

x <- seq(0.0, 0.1, length.out = 1000)

plot(x,
     dbeta(x, posterior1["alpha"], posterior1["beta"]),
     type="l", col="blue", lwd=2,
     ylim=c(0, max(
       dbeta(x, posterior1["alpha"], posterior1["beta"]),
       dbeta(x, posterior2["alpha"], posterior2["beta"]),
       dbeta(x, posterior3["alpha"], posterior3["beta"])
     )),
     ylab="density", xlab="p",
     main="Posterior comparison under different priors")

lines(x,
      dbeta(x, posterior2["alpha"], posterior2["beta"]),
      col="red", lwd=2)

lines(x,
      dbeta(x, posterior3["alpha"], posterior3["beta"]),
      col="green", lwd=2)

legend("topright",
       legend=c("Beta(1,1)", "Beta(1,19)", "Beta(5,95)"),
       col=c("blue","red","green"),
       lwd=2)
expected_of_anomalies <- sum(data$Y) / length(data$Y)
abline(v = expected_of_anomalies, col="black", lwd=2, lty=2)

ci1 <- qbeta(c(0.025, 0.975), posterior1["alpha"], posterior1["beta"])

ci2 <- qbeta(c(0.025, 0.975), posterior2["alpha"], posterior2["beta"])

ci3 <- qbeta(c(0.025, 0.975), posterior3["alpha"], posterior3["beta"])

