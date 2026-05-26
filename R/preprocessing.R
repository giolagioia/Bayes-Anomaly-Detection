library(readxl)
data <- read_excel("bank.xlsx")

#Remove unused coloumns
data$. <- NULL
data$CHQ.NO.<- NULL #usefulness feature

#Change to numeric value format and names
data$`WITHDRAWAL AMT` <- as.numeric(data$`WITHDRAWAL AMT`)
data$`DEPOSIT AMT` <- as.numeric(data$`DEPOSIT AMT`)
names(data)[names(data) == "BALANCE AMT"] <- "balance"

#Change date format
data$time_index <- as.numeric(data$DATE - min(data$DATE))
data$log_time <- log1p(data$time_index)

#Add new features
data$amount <- ifelse(
  !is.na(data$'WITHDRAWAL AMT') & data$'WITHDRAWAL AMT' > 0,
  data$'WITHDRAWAL AMT',
  data$'DEPOSIT AMT'
)

data$is_withdrawal <- ifelse(
  !is.na(data$'WITHDRAWAL AMT') & data$'WITHDRAWAL AMT' > 0,
  1,
  0
)

data$weekday <- weekdays(data$DATE)
data$weekday <- as.factor(data$weekday)

data$balance_amount_ratio <- data$balance / (abs(data$amount) + 1e-6)

#Standardization of columns
data$balance <- as.numeric(scale(data$balance))
data$amount <- as.numeric(scale(data$amount))
data$log_time <- as.numeric(scale(data$log_time))
data$balance_amount_ratio <- as.numeric(scale(data$balance_amount_ratio))
