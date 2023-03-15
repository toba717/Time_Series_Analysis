# Takao Oba
# 205615894
# HW3

########################################################################
############################ Question 1 ################################
########################################################################

library(Quandl)
library(dygraphs)
Quandl.api_key("")

retail_electronic = Quandl(code="FRED/RSEASN",type="ts", collapse="monthly", meta=TRUE)
# Do not include 2020 and 2021 years in our data
electronic = window(retail_electronic, end = c(2019, 12))

library(TSstudio) 
df=ts_to_prophet(log(electronic)) #notice that we log electronic data
names(df)= c("date", "y") # ds is the date in date format now

library(dplyr)
df$lag12=dplyr::lag(df$y, n=12)  #seasonal lag of the series
head(df, 20)  # notice the new variable "lag12" and NAs at the beginning
tail(df,20)  # no missing values at the end
### we lost 12 observations, so remove rows with na in the y
df <- df %>%
  filter(!is.na(lag12))   # remove missing values

## extract the month as a feature 

#install.packages("lubridate")
library(lubridate)

df$month= factor(month(df$date, label=TRUE), ordered=FALSE)  # month 

## create a polynomial trend feature 
df$trend=1:nrow(df)    # trend 

####################################
# Split into training and test set #
####################################


train_df=df[1:(nrow(df)-12),]
train_df
test_df=df[(nrow(df)-12+1):nrow(df), ]  



logElectronic = log(electronic) # taking the log of electronic data
par=ts_split(logElectronic, sample.out=12) 
train = train_df$y # training data
test = test_df$y # testing data

# model generation
model1= auto.arima(train,   #autoarima requires a ts object
                   xreg=cbind(model.matrix(~month, train_df)[,-1],
                              train_df$trend, 
                              train_df$lag12), 
                   seasonal=TRUE, 
                   stepwise=FALSE, 
                   approximation=FALSE)

summary(model1)
checkresiduals(model1)  # residuals have acf of white noise. 


# forecasting with model
# make sure to keep everything in the same order
fcast1 = forecast(model1, xreg=cbind(model.matrix(~month, test_df)[,-1],
                                     test_df$trend, 
                                     test_df$lag12))

# forecast values
fcast1$mean

# accuracy metrics
accuracy(fcast1, test)

# changing it back to the original values as we initially pre-transformed by taking the log
exp(fcast1$mean)





####################################
#  Acquaint yourself with the data #
####################################


class(AirPassengers)  # a ts object in Base R
head(AirPassengers) # data has timestamp (date) and variable
# install.packages("TSstudio")
library(TSstudio)
ts_info(AirPassengers)

# We will use the ts_info() function of the TSstudio package


# install.packaged("dygraphs")
library(dygraphs)
dygraph(AirPassengers, main="Air Passengers",
        ylab="Passengers (thousands)") %>% dyRangeSelector
# it seems that a transformation to stabilize variance is needed
dygraph(log(AirPassengers), main="Air Passengers",
        ylab="Log Passengers") %>% dyRangeSelector

####################################
# Convert ts object to data frame  #
####################################

#install.packages("TSstudio")
library(TSstudio)
df=ts_to_prophet(log(AirPassengers)) #notice that we log AirPAssengers
head(df)  # notice how the data is formatted now
## For prophet(), the names of the variables must stay as ds and y
## Prophet is used in other programs
str(df)  # see what type of variables you have
## for other uses, it is better to rename the date, to know what it is
names(df)= c("date", "y") # ds is the date in date format now
head(df, 20)



####################################
# Features engineering             #
####################################

## since strong annual seasonality, get the seasonal lag 12 of the variable
## and then remove the rows with missing values that doing that creates

#install.packages("dplyr")
library(dplyr)
df$lag12=dplyr::lag(df$y, n=12)  #seasonal lag of the series
head(df, 20)  # notice the new variable "lag12" and NAs at the beginning
tail(df,20)  # no missing values at the end
### we lost 12 observations, so remove rows with na in the y
df <- df %>%
  filter(!is.na(lag12))   # remove missing values
head(df, 20)
str(df)



## extract the month as a feature

#install.packages("lubridate")
library(lubridate)

df$month= factor(month(df$date, label=TRUE), ordered=FALSE)  # month
head(df,20)

## create a polynomial trend feature
df$trend=1:nrow(df)    # trend

head(df,20)

####################################
# Split into training and test set #
####################################


train_df=df[1:(nrow(df)-12),]
train_df
test_df=df[(nrow(df)-12+1):nrow(df), ]


###########################################################
# Train (fit) the regression model with the training data #
###########################################################

model1 = lm(y~trend+month+lag12, data=train_df)
summary(model1)
sdresiduals=(model1$residuals-mean(model1$residuals))/sd(model1$residuals)
plot(sdresiduals, type="l")
sdresiduals.ts=ts(sdresiduals)
plot(sdresiduals.ts, type="l")
acf(sdresiduals.ts,50, main="Residuals are autocorrelated")
pacf(sdresiduals.ts, 50, main="With ACF and PACF we might start with AR(1) model")
#### the residual plot shows strong autocorrelation in the
## residuals. They are not the residuals of a white noise
## series, as they should be for residuals of a model.
# The model has not captured all the features of the time series
## data.




#####################################################################
# Exercise 1. With the residulas just obtained, apply the gls       #
# approach learned in class. First estimate an AR(1) model for the  #
# residuals. Check whether the residuals of this residual model are #
# white noise......                                                 #
# Try other arma models if the AR(1) does not produce that.          #
# With whatever model you end up with for the residuals, do GLS, i.e #
# fit the regression model assuming your model for the residuals and #
# obtain the residuals of the whole fitted model (look at programs   #
# posted in class for GLS, to make sure that you plot the right
# residuals after you fit the gls model                             #
#####################################################################

# First estimate an AR(1) model for the residuals
# AR(1)
ar1_model <- arima(model1$residuals, order = c(1,0,0))
summary(ar1_model)
acf(ar1_model$residuals) # white noise
Box.test(ar1_model$residuals, lag = 50, type="Ljung")
# AR(1) is white noise

# ARIMA(2,0)
arima20_model <- arima(model1$residuals, order = c(2,0,0))
summary(arima20_model)
acf(arima20_model$residuals)
Box.test(arima20_model$residuals, lag = 50, type="Ljung")
# ARIMA(2,0) is white noise

# ARIMA(0,1) model
arima01_model <- arima(model1$residuals, order = c(0,0,1))
summary(arima01_model)
acf(arima01_model$residuals)
Box.test(arima01_model$residuals, lag = 50, type="Ljung")
# ARIMA(0,1) is not white noise

# ARIMA(0,2) model
arima02_model <- arima(model1$residuals, order = c(0,0,2))
summary(arima02_model)
acf(arima02_model$residuals)
Box.test(arima02_model$residuals, lag = 50, type="Ljung")
# ARIMA(0,2) is not white noise

# ARIMA(1,1) model
arima11_model <- arima(model1$residuals, order = c(1,0,1))
summary(arima11_model)
acf(arima11_model$residuals)
Box.test(arima11_model$residuals, lag = 50, type="Ljung")
# ARIMA(1,1) is white noise

# ARMA(2,1) model
arima21_model <- arima(model1$residuals, order = c(2,0,1))
summary(arima21_model)
acf(arima21_model$residuals)
Box.test(arima21_model$residuals, lag = 50, type="Ljung")
# ARMA(2,1) is white noise

# ARIMA(2,0) yielded the smallest AIC value, thus we will go with this
# the summary of ARIMA(1,1) gave us coefficient of following: 0.8579, -0.2217

# Fit GLS model with ARMA(1,1) residuals
library(nlme)
arima20_gls <- gls(y ~ trend + month + lag12, data = train_df, correlation = corARMA(c(0.6124, 0.2027), p = 2))
summary(arima20_gls)

# Get residuals of whole fitted model
gls_residuals <- residuals(arima20_gls, type = "normalized")

# acf of residual of gls model
acf(gls_residuals, lag = 50)




## Adding other economic or demographic variables is costly, and,
## in order to forecast Airpassengers we would need to forecast
## aslso the independent variables,
## A solution is then to use an ARIMA model for the residuals

#########################################################################
# Remodeling, this time with ARIMA errors and forecast package function  #
#########################################################################

par(mfrow=c(2,1))
acf(sdresiduals.ts, 50)
pacf(sdresiduals.ts, 50)
dev.off()
### will let the automatic forecast package function auto.arima decide which
## model to fit to the residuals of the regression, But to do that, the
## data frame will not
## be good. The function auto.arima was created for ts objects (and like
## arima() it does not accept factor variables), hence the little modifications
## to what we did earlier.

## Let's use the automated features of the forecast package to split
## the ts class logAirPassengers

# we will take the log since variance is not constant, and it shows in the
# model residuals.
logAirPassengers=log(AirPassengers)
class(logAirPassengers)

#install.packages("forecast")
library(forecast)

par=ts_split(logAirPassengers, sample.out=12)
train=par$train
test=par$test

## We will be using the features that we created with the data frame.
## Remember that we lost 12 first observations when creating the lag
## function. So we must correct for that. we window the trend data

train =window(train,start=c(1950,1))
train
test

########## Notice how we convert the factor month of the data frame
### to binary variables (dummies) and remove one to avoid the
## dummy variable problem

model2= auto.arima(train,   #autoarima requires a ts object
                   xreg=cbind(model.matrix(~month, train_df)[,-1],
                              train_df$trend,
                              train_df$lag12),
                   seasonal=TRUE,
                   stepwise=FALSE,
                   approximation=FALSE)
summary(model2)
checkresiduals(model2)  # residuals have acf of white noise.
summary(model2)


#####################################################################
# Question 2.                                                        #
# Compare the model that you chose in Question 1 and the results you #
# obtained with it after you apply gls with the results obtained with #
# the auto.arima function of the forecast package.                   #
# Do you reach the same conclusions? Explain and compare             #
# ###################################################################

fcast1 = predict(arima20_gls, test_df[,-c(1,2)])
fcast1
accuracy(fcast1, test)
# RMSE is 0.06583637


fcast2 = forecast(model2, xreg=cbind(model.matrix(~month, test_df)[,-1],
                                     test_df$trend,
                                     test_df$lag12))
fcast2
accuracy(fcast2, test)
# RMSE is 0.04538972

# We have that the auto.arima forecast yields a smaller RMSE value
# Thus, model2 seems to perform better of predicting on the testing data
# This reaches the same conclusion.
# In question 1, we have yielded a smaller AIC value for the auto.arima compared to the gls model
# This is intuitive as the auto.arima taken into the seasonal component "ARIMA(2,0,0)(2,0,0)[12]" but the gls model was constructed without the seasonal component
# Thus, the auto.arima model obtains a lower RMSE score compared to the gls which aligns with how it had a lower AIC score in the model construction stage.





## notice that input variables to the forecast data matrix must be in the
## same order as they appear in the training model.


# the forecast obtained for the model is the forecast of the log of AirPassengers.
# to obtain the forecast and the intervals in the original units, in thousands,
# you must exponentiate both the forecast and the intervals.


######################################################################
# Question 3                                                         #
# Write code to plot the forecast and the two types of prediction   #
# intervals in the same plot as the test data and the training data #
# Give different color to the different things and put a legend.      #
#####################################################################

# overall plot including all data points
ts.plot(cbind(train, ts(as.numeric(fcast1), frequency = 12, start = c(1960,1)), fcast2$mean, test),
        col = c("black", "red", "green", "black"),
        ylab = "Amount of Airline Passengers in Thousands", xlab = "Time in Years",
        main = "Total Airline Passengers including Forecast")
abline(v = 1960.1, col = "black", lty = 2)
legend(x=1950, y=6.5, lty=c(1,1,1),
       col=c("black", "blue", "green"), text.col=c("black", "blue", "green"),
       legend=c("Amount of Airline Passengers in Thousands","GLS Model Forecast", "Auto.arima Model Forecast"), cex=0.4)


df.forecast = data.frame(ts(as.numeric(fcast1), frequency = 12, start = c(1960,1)), fcast2$mean, test)

# close up plot just the testing set
ts.plot(df.forecast, col = c("black", "red","green"),lty = c(1, 1, 1),
        main = "Forecasts of Total Airline Passengers",
        xlab = "Time in Years",
        ylab = "Amount of Airline Passengers in thousands", ylim = c(5.8, 6.5))
legend(x=0.96, y=6.49, lty=c(1,1,1),
       col=c("black", "blue", "green"), text.col=c("black", "blue", "green"),
       legend=c("Amount of Airline Passengers in Thousands","GLS Model Forecast", "Auto.arima Model Forecast"), cex=0.4)

