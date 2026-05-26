#construction of Y column for each feature using a preexisting model 
library(isotree)
X <- data[, c("amount", "log_time", "balance", "is_withdrawal", "balance_amount_ratio", "weekday")]

model <- isolation.forest(X, seed = 123, nthreads = 1)

#Score measures how much that node is anomaly compared to all others 
score <- predict(model, X)
threshold <- quantile(score, 0.95, na.rm = TRUE)

data$Y <- ifelse(score > threshold, 1, 0)

perc_ofAnomalies <- sum(data$Y) / nrow(data)

cat("\n--- ISOLATION FOREST LABELS ---\n")
cat("Total observations:", nrow(data), "\n")
cat("Detected anomalies:", sum(data$Y), "\n")
cat("Anomaly rate:", round(perc_ofAnomalies, 7), "\n")
cat("95% score threshold:", round(as.numeric(threshold), 6), "\n")
cat("Score min / median / mean / max:",
    round(min(score, na.rm = TRUE), 6), "/",
    round(median(score, na.rm = TRUE), 6), "/",
    round(mean(score, na.rm = TRUE), 6), "/",
    round(max(score, na.rm = TRUE), 6), "\n")

png(file.path("plots", "01_isolation_forest_scores.png"),
    width = 1200, height = 800, res = 150)
hist(score,
     breaks = 60,
     col = "grey80",
     border = "white",
     main = "Isolation Forest anomaly scores",
     xlab = "Anomaly score")
abline(v = threshold, col = "red", lwd = 2, lty = 2)
legend("topright",
       legend = "95th percentile threshold",
       col = "red",
       lwd = 2,
       lty = 2,
       bty = "n")
dev.off()
