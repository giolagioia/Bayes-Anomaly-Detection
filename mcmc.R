library(brms)

#Divide the data set for train and test 80% 20%
set.seed(123)
train_idx <- sample(seq_len(nrow(data)), 0.8 * nrow(data))

train <- data[train_idx, ]
test  <- data[-train_idx, ]

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
  iter = 800,
  warmup = 400
)

print(fit_bayes)

#Frequentist approach
fit_freq <- glm(
  Y ~ amount + balance + log_time + is_withdrawal,
  family = binomial(link = "logit"),
  data = train
)

summary(fit_freq)

