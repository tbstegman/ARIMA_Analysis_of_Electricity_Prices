ARIMA Analysis of Monthly Average Retail Electricity Prices
================

## Project Description

The purpose of this projects is to explore the use of Autoregressive
Integrated Moving Average (ARIMA) models to forecast the monthly average
electricity prices for the Pacific Contiguous Region.

## Load the tidyverse and forecast packages

``` r
library(tidyverse)
library(forecast)
```

## Read in the monthly average electricity data for the Pacific Contiguous Region (cents per kilawatthour)

The data was downloaded from the [U.S. Energy Information Administration
website](https://www.eia.gov/electricity/data/browser/#/topic/7?agg=0,1&geo=01&endsec=vg&linechart=ELEC.PRICE.PCC-ALL.M&columnchart=ELEC.PRICE.PCC-ALL.M&map=ELEC.PRICE.PCC-ALL.M&freq=M&ctype=linechart&ltype=pin&rtype=s&pin=&rse=0&maptype=0%60)
for the time period of Jan-2001 through Oct-2020. However, only data
through Dec-2019 was used since the 2020 data was not considered final
at the time of this analysis.

``` r
series <- read_csv("https://raw.githubusercontent.com/tbstegman/ARIMA_Analysis_of_Electricity_Prices/main/Average_retail_price_of_electricity_monthly.csv",skip=4)
```

## Perform some basic transformations and plot the data

``` r
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
```

![](https://raw.githubusercontent.com/tbstegman/ARIMA_Analysis_of_Electricity_Prices/main/graphs/tsplot-1.png)<!-- -->

From this plot it looks like the variance is fairly stable. However, we
can see both a pronounced upward trend as well as a seasonal pattern,
which indicate that the series is not stationary.

## Create a training data set, and hold out the last 24 months of data for testing

``` r
train <- window(price, start=c(2001,1), end=c(2017,12))
test <- window(price, start=c(2018,1))
```

Now, run the **ndiffs** and **nsdiffs** functions on the training set to
determine the number of differences suggested to stabilize the mean and
make the series stationary.

``` r
ndiffs(train)
```

    ## [1] 1

``` r
nsdiffs(diff(train))
```

    ## [1] 1

Based upon the output from the **ndiffs** and **nsdiffs** functions, it
looks like a first difference along with a difference at lag 12 would be
needed to make the series stationary. So, we apply these differences and
produce the time series, ACF, and PACF plots for the differenced series.

``` r
# Differnce the series
diff1 <- diff(train)
diff1_12 <- diff(diff1,lag=12)

# Create plots for the differenced data
ggtsdisplay(diff1_12)
```

![](https://raw.githubusercontent.com/tbstegman/ARIMA_Analysis_of_Electricity_Prices/main/graphs/train_diff_plt-1.png)<!-- -->

The time series plot looks more or less stationary. Since the ACF drops
off sharply at lag 1, and the PACF decays more slowly, it looks as
though a non-seasonal MA(1) term might be indicated. Also, since the ACF
has a pronounced spike at lag 12, but is below the threshold at lags 24
and 36, while the PACF drops off more slowly at these seasonal lags, a
seasonal MA(1) term might also be appropriate. Putting these pieces of
information together, it appears that an
ARIMA(0,1,1)(0,1,1)<sub>12</sub> model might be a good fit for this
series.

## Use the auto.arima function to find the best models for the AIC, AICc, and BIC information criterion

Use auto.arima to run through all possible orders with d=1 and D=1 up to
ARIMA(4,1,4)(2,1,2)<sub>12</sub> to find the best models based upon the
AICc, AIC, and BIC information criterion.

``` r
# Find the model that minimizes AICc
Arimafit1 <- auto.arima(train, d=1, D=1, ic="aicc", max.p=4, max.q=4, max.order=14, stepwise=F, approximation=F)
summary(Arimafit1)
```

    ## Series: train 
    ## ARIMA(1,1,1)(0,1,1)[12] 
    ## 
    ## Coefficients:
    ##          ar1      ma1     sma1
    ##       0.3361  -0.7050  -0.6052
    ## s.e.  0.1545   0.1228   0.0660
    ## 
    ## sigma^2 estimated as 0.139:  log likelihood=-83.94
    ## AIC=175.89   AICc=176.1   BIC=188.9
    ## 
    ## Training set error measures:
    ##                       ME      RMSE       MAE        MPE     MAPE      MASE
    ## Training set -0.02808033 0.3579259 0.2549248 -0.3551059 2.351982 0.5596976
    ##                     ACF1
    ## Training set 0.005749966

``` r
# Find the model that minimizes AIC
Arimafit2 <- auto.arima(train, d=1, D=1, ic="aic", max.p=4, max.q=4, max.order=14, stepwise=F, approximation=F)
summary(Arimafit2)
```

    ## Series: train 
    ## ARIMA(1,1,1)(0,1,1)[12] 
    ## 
    ## Coefficients:
    ##          ar1      ma1     sma1
    ##       0.3361  -0.7050  -0.6052
    ## s.e.  0.1545   0.1228   0.0660
    ## 
    ## sigma^2 estimated as 0.139:  log likelihood=-83.94
    ## AIC=175.89   AICc=176.1   BIC=188.9
    ## 
    ## Training set error measures:
    ##                       ME      RMSE       MAE        MPE     MAPE      MASE
    ## Training set -0.02808033 0.3579259 0.2549248 -0.3551059 2.351982 0.5596976
    ##                     ACF1
    ## Training set 0.005749966

``` r
# Find the model that minimizes BIC
Arimafit3 <- auto.arima(train, d=1, D=1, ic="bic", max.p=4, max.q=4, max.order=14, stepwise=F, approximation=F)
summary(Arimafit3)
```

    ## Series: train 
    ## ARIMA(0,1,1)(0,1,1)[12] 
    ## 
    ## Coefficients:
    ##           ma1     sma1
    ##       -0.4125  -0.6092
    ## s.e.   0.0803   0.0644
    ## 
    ## sigma^2 estimated as 0.141:  log likelihood=-85.8
    ## AIC=177.6   AICc=177.73   BIC=187.36
    ## 
    ## Training set error measures:
    ##                       ME      RMSE       MAE        MPE     MAPE      MASE
    ## Training set -0.02441675 0.3614281 0.2552623 -0.3167194 2.359247 0.5604386
    ##                    ACF1
    ## Training set 0.05588174

The model which minimized the AICc and AIC was found to be an
ARIMA(1,1,1)(0,1,1)<sub>12</sub> model, while the model that minimized
the BIC was ARIMA(0,1,1)(0,1,1)<sub>12</sub> (which coincides with the
model identified earlier based upon the ACF and PACF patterns).

## Create a function to check residuals, and calculate forecast accuracy metrics on the test data for a given ARIMA model fit

``` r
ARIMA_checks <- function(fit){
  checkresiduals(fit)
  accuracy(forecast(fit,h=length(test)),test)
}
```

Perform checks for the two ARIMA models identified above.

``` r
ARIMA_checks(Arimafit1)
```

![](https://raw.githubusercontent.com/tbstegman/ARIMA_Analysis_of_Electricity_Prices/main/graphs/ARIMA1_checks-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(1,1,1)(0,1,1)[12]
    ## Q* = 17.342, df = 21, p-value = 0.6902
    ## 
    ## Model df: 3.   Total lags used: 24

    ##                       ME      RMSE       MAE         MPE     MAPE      MASE
    ## Training set -0.02808033 0.3579259 0.2549248 -0.35510587 2.351982 0.5596976
    ## Test set      0.01399790 0.2745546 0.1906531  0.09302195 1.399734 0.4185866
    ##                      ACF1 Theil's U
    ## Training set  0.005749966        NA
    ## Test set     -0.257335273 0.3391123

``` r
ARIMA_checks(Arimafit3)
```

![](https://raw.githubusercontent.com/tbstegman/ARIMA_Analysis_of_Electricity_Prices/main/graphs/ARIMA3_checks-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(0,1,1)(0,1,1)[12]
    ## Q* = 21.844, df = 22, p-value = 0.4692
    ## 
    ## Model df: 2.   Total lags used: 24

    ##                       ME      RMSE       MAE        MPE     MAPE      MASE
    ## Training set -0.02441675 0.3614281 0.2552623 -0.3167194 2.359247 0.5604386
    ## Test set      0.03080864 0.2750138 0.1931496  0.2147181 1.415314 0.4240676
    ##                     ACF1 Theil's U
    ## Training set  0.05588174        NA
    ## Test set     -0.26243067 0.3405676

Both models fit the training data pretty well. The residuals for both
series appear to be white noise, and the forecast accuracy metrics
are comparable for the two models.

Now, perform the same checks for the benchmark model of
ARIMA(0,1,0)(0,1,0)<sub>12</sub> to compare its performance against the
two ARIMA models identified above.

``` r
bench <- Arima(train,order=c(0,1,0),seasonal=c(0,1,0))
ARIMA_checks(bench)
```

![](https://raw.githubusercontent.com/tbstegman/ARIMA_Analysis_of_Electricity_Prices/main/graphs/bench_checks-1.png)<!-- -->

    ## 
    ##  Ljung-Box test
    ## 
    ## data:  Residuals from ARIMA(0,1,0)(0,1,0)[12]
    ## Q* = 90.918, df = 24, p-value = 1.016e-09
    ## 
    ## Model df: 0.   Total lags used: 24

    ##                        ME      RMSE       MAE        MPE     MAPE      MASE
    ## Training set -0.009435062 0.4436648 0.3204541 -0.1412491 2.952060 0.7035699
    ## Test set      0.060833333 0.3402817 0.2516667  0.4509954 1.846049 0.5525443
    ##                    ACF1 Theil's U
    ## Training set -0.2703663        NA
    ## Test set     -0.2450455  0.427201

Based upon the diagnostic plots, Ljung-Box test, and forecast accuracy
metrics, we can see that the two ARIMA models identified earlier both
fit the data better than the benchmark model.

## Choose the best model, and produce a forecast with prediction intervals for the next 12 months

Since both the ARIMA(1,1,1)(0,1,1)<sub>12</sub> and
ARIMA(0,1,1)(0,1,1)<sub>12</sub> models have excellent diagnostics and
perform similarly well on the test set, we choose the
ARIMA(0,1,1)(0,1,1)<sub>12</sub> model for forecasting as it is the more
parsimonious of the two.

``` r
# Fit the best model to all of the data
best <- Arima(price, order=c(0,1,1), seasonal=c(0,1,1))

# Print the forecast
forecast(best,h=12)
```

    ##          Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
    ## Jan 2020       13.13672 12.65924 13.61420 12.40647 13.86697
    ## Feb 2020       13.19218 12.65398 13.73037 12.36908 14.01528
    ## Mar 2020       13.22222 12.62949 13.81494 12.31572 14.12871
    ## Apr 2020       12.51232 11.86968 13.15496 11.52948 13.49515
    ## May 2020       13.67886 12.98991 14.36781 12.62520 14.73252
    ## Jun 2020       15.02011 14.28777 15.75245 13.90009 16.14012
    ## Jul 2020       15.48587 14.71257 16.25916 14.30321 16.66852
    ## Aug 2020       15.73289 14.92070 16.54508 14.49076 16.97503
    ## Sep 2020       15.52333 14.67403 16.37264 14.22444 16.82223
    ## Oct 2020       14.03908 13.15422 14.92395 12.68580 15.39236
    ## Nov 2020       13.90964 12.99059 14.82868 12.50407 15.31520
    ## Dec 2020       13.43976 12.48775 14.39177 11.98379 14.89573

``` r
# Plot the forecast
autoplot(forecast(best,h=12)) +
  labs(x="Year",y="Avg. Price (cents per kilawatthour)") +
  theme(plot.title = element_text(hjust = 0.5,size=15))
```

![](https://raw.githubusercontent.com/tbstegman/ARIMA_Analysis_of_Electricity_Prices/main/graphs/final-1.png)<!-- -->

## References

Hyndman, R.J., & Athanasopoulos, G. (2018) Forecasting: principles and
practice, 2nd edition, OTexts: Melbourne, Australia. OTexts.com/fpp2.
Accessed on 04Jan2021.

Hyndman R, Athanasopoulos G, Bergmeir C, Caceres G, Chhay L, O’Hara-Wild
M, Petropoulos F,Razbash S, Wang E, Yasmeen F (2020). *forecast:
Forecasting functions for time series and linear models*. R package
version 8.13, \<URL: <https://pkg.robjhyndman.com/forecast/>\>.

Hyndman RJ, Khandakar Y (2008). “Automatic time series forecasting: the
forecast package for R.” *Journal of Statistical Software*, *26*(3),
1-22. \<URL: <https://www.jstatsoft.org/article/view/v027i03>\>.
