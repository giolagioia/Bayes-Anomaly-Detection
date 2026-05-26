#construction of Y column for each feature using a preexisting model 
library(isotree)
X <- data[, c("amount", "log_time", "balance", "is_withdrawal", "balance_amount_ratio", "weekday")]

model <- isolation.forest(X)

#Score measures how much that node is anomaly compared to all others 
score <- predict(model, X)
threshold <- quantile(score, 0.95, na.rm = TRUE)

data$Y <- ifelse(score > threshold, 1, 0)

perc_ofAnomalies <- sum(data$Y) / nrow(data)
