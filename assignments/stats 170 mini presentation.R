# TAKAO OBA 
# STATS 170
# forecast package exp smoothing

# exponential smoothing forecast
library(forecast)
library(dygraphs)

# class of AirPassengers should be ts
class(AirPassengers)

# look at the first data
head(AirPassengers)

# view time series through dygraph
dygraph(AirPassengers, main="Air Passengers", 
        ylab="Passengers (thousands)") %>% dyRangeSelector

# it seems that a transformation to stabilize variance is needed
dygraph(log(AirPassengers), main="Air Passengers", 
        ylab="Log Passengers") %>% dyRangeSelector

# plotting the train data                            
autoplot(AirPassengers) + ylab("Monthly Air Passenger Numbers") + xlab("Year")


#################### simple exponential smoothing ####################

# SES is the simplest form of time series forecasting model, 
# which is used for univariate time series data without any trend or seasonality. 

# Estimate parameters
fc <- ses(AirPassengers, h=12)
# Accuracy of one-step-ahead training errors
round(accuracy(fc),2)
# ME  RMSE   MAE  MPE MAPE MASE ACF1
# Training set 2.05 28.56 21.98 0.42 8.78 0.77 0.29


fc2 <- holt(AirPassengers, damped=TRUE, phi = 0.9, h=12)
round(accuracy(fc2),2)
# ME  RMSE   MAE  MPE MAPE MASE ACF1
# Training set 1.83 28.56 22.06 0.24 8.86 0.77 0.29


# simple exponential smoothing PI = FALSE
autoplot(AirPassengers) +
  autolayer(fc, series="Holt's method", PI=FALSE) +
  autolayer(fc2, series="Damped Holt's method", PI=FALSE, lty = 2) +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))


# simple exponential smoothing no confidence interval
autoplot(AirPassengers) +
  autolayer(fc, series="Holt's method") +
  autolayer(fc2, series="Damped Holt's method") +
  ggtitle("Forecasts from Holt's method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour=guide_legend(title="Forecast"))

# notice that simple exponential smoothing doesn't take into account seasonality and trend
# the predicted value is a straight horizontal line 


#################### trend models ####################

# The Holt method is used for time series data that have a trend but no seasonality. 
# The Holt function includes two components: the level and the slope or trend.

# trend method
fc <- holt(AirPassengers, h = 12)
fc2 <- holt(AirPassengers, h = 12, damped = TRUE, phi = 0.9)
# the damped parameter changes the trend to flatten out eventually


# with PI = false parameter
autoplot(AirPassengers) +
  autolayer(fc, series = "Holt's method", PI = F) +
  autolayer(fc2, series = "Damped Holt's method", PI = F) +
  ggtitle("Forecasts from Holt's Method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour = guide_legend(title = "Forecast"))


# without PI = false parameter
autoplot(AirPassengers) +
  autolayer(fc, series = "Holt's method") +
  autolayer(fc2, series = "Damped Holt's method", lty = 2) +
  ggtitle("Forecasts from Holt's Trend Method") + xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

# notice that we can see that the model captures the trend as well.

e1 <- tsCV(AirPassengers, ses, h=1)
e2 <- tsCV(AirPassengers, holt, h=1)
e3 <- tsCV(AirPassengers, holt, damped=TRUE, h=1)
# Compare MSE:
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)
mean(e3^2, na.rm=TRUE)

# Compare MAE:
mean(abs(e1), na.rm=TRUE)
mean(abs(e2), na.rm=TRUE)
mean(abs(e3), na.rm=TRUE)


#################### consider holt winters ####################

# The Holt-Winters method is an extension of SES that adds a trend and seasonality component to the model. 

# additive model
fit1 <- hw(AirPassengers,seasonal="additive")
fit1[["model"]]
# multiplicative model
fit2 <- hw(AirPassengers,seasonal="multiplicative")
fit2[["model"]]

# plot of non-damped
autoplot(AirPassengers) +
  autolayer(fit1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  ggtitle("Forecast from Holt Winters") +
  guides(colour=guide_legend(title="Forecast"))

# additive model damped
fit1_damped <- hw(AirPassengers, damped = TRUE, seasonal="additive")
# multiplicative model damped
fit2_damped <- hw(AirPassengers, damped = TRUE, seasonal="multiplicative")

# plot of damped
autoplot(AirPassengers) +
  autolayer(fit1_damped, series="HW additive forecasts", PI=FALSE) +
  autolayer(fit2_damped, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Air passengers in Australia (millions)") +
  ggtitle("Forecast from Holt Winters") +
  guides(colour=guide_legend(title="Forecast"))

# notice that we can see that the model captures seasonality as well as trends


# subset 35 of the latest data and see how the predictions work
fc <- hw(subset(AirPassengers,end=length(AirPassengers)-35),
         damped = TRUE, seasonal="multiplicative", h=35)
autoplot(AirPassengers) +
  autolayer(fc, series="HW multi damped", PI=FALSE)+
  guides(colour=guide_legend(title="Daily forecasts"))


#################### estimation and model selection ####################
# Error, trend, and seasonality
fit <- ets(AirPassengers)
summary(fit)

# note that this outputs the AIC, AIC_c, and BIC
# AIC is based on likelihood function of the model and number of parameters in model
# BIC has a larger penalty value 
# AIC_C is a modified version of AIC that is more appropriate for small sample sizes

# looking at residuals and forecast errors
cbind('Residuals' = residuals(fit),
      'Forecast errors' = residuals(fit,type='response')) %>%
  autoplot(facet=TRUE) + xlab("Year") + ylab("")


#################### Forecasting with ETS models ####################

# An ETS model is specified using three parameters:
#   
# E: The error or noise component, which is modeled as a random process with zero mean and constant variance.
# 
# T: The trend component, which is modeled using either an additive or multiplicative model, depending on whether the trend is linear or exponential.
# 
# S: The seasonality component, which is modeled using either an additive or multiplicative model, depending on whether the seasonal patterns are consistent over time or change over time.

# ETS point forecasts are equal to the medians of the forecast distribution
fit %>% forecast(h=8) %>%
  autoplot() +
  ylab("Air passengers in Australia (millions)")

# forecasts points
forecast(fit)
