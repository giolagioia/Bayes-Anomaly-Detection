library(readxl)
data <- read_excel("bank.xlsx")

#Remove unused coloumns
data$. <- NULL
data$CHQ.NO.<- NULL #usefulness feature

#Change to numeric value format
data$`WITHDRAWAL AMT` <- as.numeric(data$`WITHDRAWAL AMT`)
data$`DEPOSIT AMT` <- as.numeric(data$`DEPOSIT AMT`)

data$amount <- ifelse(
  !is.na(data$'WITHDRAWAL AMT') & data$'WITHDRAWAL AMT' > 0,
  -data$'WITHDRAWAL AMT',
  data$'DEPOSIT AMT'
)

data$is_withdrawal <- ifelse(data$amount < 0, 1, 0)

#Change date format
data$time_index <- as.numeric(data$DATE - min(data$DATE))
data$log_time <- log1p(data$time_index)

#Change coloumn names
names(data)[names(data) == "BALANCE AMT"] <- "balance"

#Standardization of coloumns
data$balance <- as.numeric(scale(data$balance))
data$amount <- as.numeric(scale(data$amount))
data$log_time <- as.numeric(scale(data$log_time))

