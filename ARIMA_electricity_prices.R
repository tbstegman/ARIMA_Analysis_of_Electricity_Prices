# Load the tidyverse and forecast packages
library(tidyverse)
library(forecast)

# Read in the monthly average electricity data 
# for the Pacific Contiguous Region (cents per kilawatthour)
series <- read_csv("https://raw.githubusercontent.com/tbstegman/ARIMA_Analysis_of_Electricity_Prices/main/Average_retail_price_of_electricity_monthly.csv",skip=4)

# Rename the variables
names(series) <- c("month","avg_price")

# Convert month to Date class
series$month <- as.Date.character(paste("01",series$month),format="%d %b %Y")

# Sort the data by ascending month
series <- series[order(series$month),]

# Create a time series object of the monthly data but exclude the last 12 months
price <- ts(series$avg_price,frequency=12,start=c(2001,1)) %>% window(end=c(2019,12))

# Plot the time series
autoplot(price) +
  labs(x="Year",y="Avg. Price (cents per kilawatthour)") +
  ggtitle("Average Electricity Prices for the Pacific Contiguous Region") +
  theme(plot.title = element_text(hjust = 0.5,size=15))

# Create a training data set, and hold out the last 24 months of data for testing
train <- window(price, start=c(2001,1), end=c(2017,12))
test <- window(price, start=c(2018,1))

# Run the ndiffs and nsdiffs functions on the training set to determine the number 
# of differences suggested to stabilize the mean
ndiffs(train)
nsdiffs(diff(train))

# Differnce the series
diff1 <- diff(train)
diff1_12 <- diff(diff1,lag=12)

# Create plots for the differenced data
ggtsdisplay(diff1_12)

# Find the model that minimizes AICc
Arimafit1 <- auto.arima(train, d=1, D=1, ic="aicc", max.p=4, max.q=4, max.order=14, stepwise=F, approximation=F)
summary(Arimafit1)

# Find the model that minimizes AIC
Arimafit2 <- auto.arima(train, d=1, D=1, ic="aic", max.p=4, max.q=4, max.order=14, stepwise=F, approximation=F)
summary(Arimafit2)

# Find the model that minimizes BIC
Arimafit3 <- auto.arima(train, d=1, D=1, ic="bic", max.p=4, max.q=4, max.order=14, stepwise=F, approximation=F)
summary(Arimafit3)

# Create a function to check residuals, and calculate forecast accuracy metrics 
# on the test data for a given ARIMA model fit
ARIMA_checks <- function(fit){
  checkresiduals(fit)
  accuracy(forecast(fit,h=length(test)),test)
}

# Perform checks for the two ARIMA models identified above.
ARIMA_checks(Arimafit1)
ARIMA_checks(Arimafit3)

# Now, perform the same checks for the benchmark model of ARIMA(0,1,0)(0,1,0)[12] 
# to compare its performance against the two ARIMA models identified above
bench <- Arima(train,order=c(0,1,0),seasonal=c(0,1,0))
ARIMA_checks(bench)

# Fit the best model to all of the data
best <- Arima(price, order=c(0,1,1), seasonal=c(0,1,1))

# Print the forecast for 12 months
forecast(best,h=12)

# Plot the forecast
autoplot(forecast(best,h=12)) +
  labs(x="Year",y="Avg. Price (cents per kilawatthour)") +
  theme(plot.title = element_text(hjust = 0.5,size=15))






