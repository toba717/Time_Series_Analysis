####################################################################
####################################################################
# Takao Oba, Anish Dulla, Daniel Neufeldt, Shoichiro Ueno
# Retail Sales Time Series Analysis
# STATS 170
####################################################################
####################################################################

####################################################################
# Models Introduced:
# # # Exponential Smoothing
# # # Time Series Regression
# # # ARIMA Modeling Forecast
# # # VAR Forecast
# # # Forecast Package Auto.ARIMA
# # # Classical Machine Learning Regression 
# # # Random Forest
# # # Gradient Boosting
# # # Prophet
####################################################################

#------------------------------------------------------------------#
####################################################################
######################### I. Introduction ##########################
####################################################################
#------------------------------------------------------------------#
library(Quandl)
library(dygraphs)
Quandl.api_key("Rtc_Eon27jXhwyGeEfyr")

retail_electronic = Quandl(code="FRED/RSEASN",type="ts", collapse="monthly", meta=TRUE)
# Do not include 2020 and 2021 years in our data
electronic = window(retail_electronic, end = c(2019, 12))
# > 200 observations & start: 1992 1

retail_hobby = Quandl(code = "FRED/RSSGHBMSN", type = "ts", collapse = "monthly", meta = TRUE)
# Do not include 2020 and 2021 years in our data
hobby = window(retail_hobby, end = c(2019, 12))
# > 200 observations & start: 1992 1
start(hobby)

retail_furniture = Quandl(code = "FRED/RSFHFSN", type = "ts", collapse = "monthly", meta = TRUE)
# Do not include 2020 and 2021 years in our data
furniture = window(retail_furniture, end = c(2019, 12))
# > 200 observations & start: 1992 1

retail_sales <- cbind(electronic, hobby, furniture)
dygraph(retail_sales, main = "Electronic, Hobby, and Furniture Sales Over Time",
        ylab = "Sales (in Millions of Dollars)", xlab = "Time (in years)") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2")) %>%
  dyRangeSelector()

retails_electronic_train = electronic %>% 
  window(end = c(2018, 12))

retails_electronic_test = electronic %>% 
  window(start = 2019)

#------------------------------------------------------------------#
####################################################################
######## II. Components Features of the dependent variable #########
####################################################################
#------------------------------------------------------------------#

plot(retails_electronic_train)

add_decomp <- (decompose(retails_electronic_train, type='add')) 
mult_decomp <- decompose(retails_electronic_train, type = "mult")

plot(add_decomp)
plot(mult_decomp)
# multiplicative decomposition is better

boxplot_elec <- boxplot(retails_electronic_train ~ cycle(retails_electronic_train),
                        main = "Seasonal Boxplot of Retail Electronic Sales (Training)",
                        xlab = "Month",
                        ylab = "Retail Electronic Sales")


#------------------------------------------------------------------#
####################################################################
##### III. Autocorrelation features of the random term of the ######
############## dependent variable in the training set ##############
####################################################################
#------------------------------------------------------------------#


mult_decomp_random_term <- mult_decomp$random

par(mfrow=c(2, 1))
acf(window(mult_decomp_random_term, start = c(1992, 7), end = c(2018, 6)), main = "ACF - Multiplicative Decomposition \n of Random Term: Electronic Retail Sales")
pacf(window(mult_decomp_random_term, start = c(1992, 7), end = c(2018, 6)), main = "PACF - Multiplicative Decomposition \n of Random Term: Electronic Retail Sales")


#------------------------------------------------------------------#
####################################################################
######## IV. Exponential smoothing modeling and forecasting ########
####################################################################
#------------------------------------------------------------------#

electronic_smooth <- HoltWinters(retails_electronic_train, seasonal = "multiplicative")
plot(electronic_smooth, main = "Exponential Smoothing Model of Electronic Retail Sales", xlab = "Year", ylab = "Retail Sales (in Millions of Dollars)")
legend(2011, 15999, legend=c("Electronic Retail Sales", "Fitted Exp. Smoothing"), col=c("black", "red"), lty=1, cex=0.6)

electronic_pred <- predict(electronic_smooth, n.ahead = 12) # predict with smoothing model
electronic_pred

plot(electronic_smooth, electronic_pred, main = "Exponential Smoothing of Electronic Retail Sales With Forecasting",
     xlab = "Year", ylab = "Retail Sales (in Millions of Dollars)")
lines(retails_electronic_test, col = "blue", lty = 2)
legend(1993, 15900, legend = c("Electronic Retail Sales (Train)", "Electronic Retail Sales (Test)", "Fitted Exp. Smoothing"), col = c("black", "blue", "red"), lty = c(1, 2, 1), cex = 0.6)


par(mfrow = c(1, 2))
plot(electronic_pred, main = "Exponential Smoothing\n Forecast vs Retail \nElectronic Sales", xlab = "Time (2019)", ylab = "Retail Sales (in millions of dollars)", col = "red")
lines(retails_electronic_test, col = "blue", lty = 2)
legend(2019, 10900, legend = c("Electronic Retail Sales", "Fitted Exp. Smoothing") , col = c("blue", "red"), lty = 2:1, cex = 0.6)

# plot residuals of exp smoothing model vs testing data
residual = as.numeric(retails_electronic_test) - as.numeric(electronic_pred)
plot(residual, main = "Residuals of Exponential\n Smoothing Forecast", xlab = "Time (in months) -- 2019", ylab = "Residuals (in millions of dollars)")
abline(h = 0, col = "red")

### V. Polynomial regression plus seasonal effect modeling and forecasting


#------------------------------------------------------------------#
####################################################################
########## V. Polynomial regression plus seasonal effect ###########
##################### modeling and forecasting #####################
####################################################################
#------------------------------------------------------------------#


mult_decomp_seasonal_term <- mult_decomp$seasonal

regression_df <- data.frame(as.numeric(time(retails_electronic_train)))
regression_df$electronic_sales <- as.numeric(retails_electronic_train)
colnames(regression_df)[1] <- "time"
regression_df$time2 <- regression_df$time ^ 2
regression_df$time3 <- regression_df$time ^ 3

head(regression_df)

# try three different polynomial regression models
lin_model <- lm(electronic_sales ~ time, data = regression_df)
quad_model <- lm(electronic_sales ~ time + time2, data = regression_df)
cubic_model <- lm(electronic_sales ~ time + time2 + time3, data = regression_df)

lin_model_fit <- as.numeric(predict(lin_model, data = regression_df))
quad_model_fit <- as.numeric(predict(quad_model, data = regression_df))
cubic_model_fit <- as.numeric(predict(cubic_model, data = regression_df))

# after estimating trend, multiply by seasonal component
seasonals <- as.numeric(mult_decomp$seasonal)[1:12]
total_lin_model_fit <- lin_model_fit * seasonals
total_quad_model_fit <- quad_model_fit * seasonals
total_cubic_model_fit <- cubic_model_fit * seasonals

lin_residuals <- as.numeric(retails_electronic_train) - total_lin_model_fit
quad_residuals <- as.numeric(retails_electronic_train) - total_quad_model_fit
cubic_residuals <- as.numeric(retails_electronic_train) - total_cubic_model_fit

scaled_lin_residuals <- scale(lin_residuals)
scaled_quad_residuals <- scale(quad_residuals)
scaled_cubic_residuals <- scale(cubic_residuals)

par(mfrow=c(1,3))

plot(ts(scaled_lin_residuals), main = "Scaled Residual Plot - Linear Model", ylab = "Standarized Residuals", xlab = "Time (in months)")
abline(h = 0, col = "red")

plot(ts(scaled_quad_residuals), main = "Scaled Residual Plot - Quadratic Model", ylab = "Standarized Residuals", xlab = "Time (in months)")
abline(h = 0, col = "red")

plot(ts(scaled_cubic_residuals), main = "Scaled Residual Plot - Cubic Model", ylab = "Standarized Residuals", xlab = "Time (in months)")
abline(h = 0, col = "red")

testTimeRE <- data.frame(as.numeric(time(retails_electronic_test)))
colnames(testTimeRE)[1] <- "time"
testTimeRE$time2 <- testTimeRE$time ^ 2
testTimeRE$time3 <- testTimeRE$time ^ 3

predictions_lin <- predict(lin_model, newdata=testTimeRE)
predictions_quad <- predict(quad_model, newdata=testTimeRE)
predictions_cubic <- predict(cubic_model, newdata=testTimeRE)

seasonals <- as.numeric(mult_decomp$seasonal)[1:12]
total_predictions_lin <- predictions_lin * seasonals
total_predictions_quad <- predictions_quad * seasonals
total_predictions_cubic <- predictions_cubic * seasonals

plot(retails_electronic_test - total_predictions_lin)
abline(h = 0, col = "red")

plot(retails_electronic_test - total_predictions_quad)
abline(h = 0, col = "red")

plot(retails_electronic_test - total_predictions_cubic)
abline(h = 0, col = "red")

RMSE_lin <- sqrt(mean((retails_electronic_test - total_predictions_lin)^2))
RMSE_quad <- sqrt(mean((retails_electronic_test - total_predictions_quad)^2))
RMSE_cubic <- sqrt(mean((retails_electronic_test - total_predictions_cubic)^2))

min(RMSE_lin, RMSE_quad, RMSE_cubic) #RMSE_quad is smallest


#------------------------------------------------------------------#
####################################################################
########################### VI. Conclusion #########################
####################################################################
#------------------------------------------------------------------#

RMSE_exp_smooth <- sqrt(mean((retails_electronic_test - electronic_pred)^2))
RMSE_exp_smooth
average_forecast <- (total_predictions_quad + electronic_pred) / 2
average_RMSE <- mean(c(RMSE_exp_smooth, RMSE_quad))


#------------------------------------------------------------------#
####################################################################
############## VII. ARIMA modeling and forecasting #################
####################################################################
#------------------------------------------------------------------#

### Section VII.1. Address the need for pre-transformations

y <- retails_electronic_train

par(mfrow=c(3, 1))
plot(y ^ (1/2), main = "Sqrt Transformation of Electronic Retail Sales",
     xlab = "Time (in years)", ylab = "Sales (in millions)")
plot(y ^ (1/4), main = "Quartic Transformation of Electronic Retail Sales",
     xlab = "Time (in years)", ylab = "Sales (in millions)")
plot(log(y), main = "Log Transformation of Electronic Retail Sales",
     xlab = "Time (in years)", ylab = "Sales (in millions)")

y.star <- log(y)

### Section VII.2. Assessment of mean stationarity

par(mfrow = c(2,1))
acf(y.star, lag = 50, main = "ACF of Electronic Retail Sales")
pacf(y.star, lag = 50, main = "PACF of Electronic Retail Sales")

reg_diff_y.star <- diff(y.star, lag = 1, differences = 1)
seasonal_diff_y.star <- diff(y.star, lag = 12, differences = 1)
reg_seasonal_diff_y.star <- diff(reg_diff_y.star, lag = 12)

par(mfrow=c(2, 1))
acf(reg_diff_y.star, lag = 50, main = "ACF - Regular Differencing: Electronic Retail Sales")
pacf(reg_diff_y.star, lag = 50, main = "PACF - Regular Differencing: Electronic Retail Sales")

par(mfrow=c(2, 1))
acf(seasonal_diff_y.star, lag = 50, main = "ACF - Seasonal Differencing: Electronic Retail Sales")
pacf(seasonal_diff_y.star, lag = 50, main = "PACF - Seasonal Differencing: Electronic Retail Sales")

par(mfrow=c(2, 1))
acf(reg_seasonal_diff_y.star, lag = 50, main = "ACF - Regular and Seasonal Differencing: Electronic Retail Sales")
pacf(reg_seasonal_diff_y.star, lag = 50, main = "PACF - Regular and Seasonal Differencing: Electronic Retail Sales")

y.star.star <- reg_seasonal_diff_y.star

### Section VII.3. Identification

electronic_arima_model1 <- arima(y.star, order = c(0, 1, 1), seas = list(order = c(1, 1, 1), 12))

### Section VII.4 Fit and diagnose

par(mfrow=c(2,1))
acf(electronic_arima_model1$residuals, lag = 50, main = "ACF - Residuals of ARIMA(0,1,1)(1,1,1)[12]: Electronic Retail Sales")
pacf(electronic_arima_model1$residuals, lag = 50, main = "PACF - Residuals of ARIMA(0,1,1)(1,1,1)[12]: Electronic Retail Sales")
Box.test(electronic_arima_model1$residuals, lag = 12, type="Ljung") # lag ? 12 or 20
electronic_arima_model1$aic

electronic_arima_model2 <- arima(y.star, order = c(0, 1, 1), seas = list(order = c(0, 1, 2), 12))

par(mfrow=c(2,1))
acf(electronic_arima_model2$residuals, lag = 50, main = "ACF - Residuals of ARIMA(0,1,1)(0,1,2)[12]: Electronic Retail Sales")
pacf(electronic_arima_model2$residuals, lag = 50, main = "PACF - Residuals of ARIMA(0,1,1)(0,1,2)[12]: Electronic Retail Sales")
Box.test(electronic_arima_model2$residuals, lag = 12, type="Ljung") # lag ? 12 or 20
electronic_arima_model2$aic

# comes from electronic_arima_model1 coefficients
Mod(polyroot(c(1, -0.1843))) # Stationarity
Mod(polyroot(c(1, -0.3745))) # Invertibility
Mod(polyroot(c(1, -0.5782))) # Invertibility

all(c(Mod(polyroot(c(1, -0.3745))), Mod(polyroot(c(1, -0.1843))), Mod(polyroot(c(1, -0.5782)))) > 1)

# T-Test Statistics for Parameters 
arima_coefficients_model1 <- electronic_arima_model1$coef
arima_se_model1 <- sqrt(diag(vcov(electronic_arima_model1)))
t_test_arima_model1 <- arima_coefficients_model1 / arima_se_model1
t_test_arima_model1
abs(t_test_arima_model1) > 2 # test for significance

### Section VII.5-Forecasting

forecast <- predict(electronic_arima_model1, n.ahead = 12)
forecast.value <- ts((forecast$pred), start=start(retails_electronic_test), freq=12)
ci.low <- ts((forecast$pred-1.96*forecast$se), start=start(retails_electronic_test),freq=12)
ci.high=ts((forecast$pred+1.96*forecast$se), start=start(retails_electronic_test),freq=12)

df.forecast = data.frame(retails_electronic_test, exp(forecast.value), 
                         exp(ci.low), exp(ci.high), forecast$se) #exp due to log transform

df.forecast

ts.plot(cbind(retails_electronic_train, exp(ci.low), exp(forecast.value),
              retails_electronic_test, exp(ci.high)),
        col=c("black", "blue", "red", "black" ,"blue"),
        lty = c(1,3,1,1,2), main="Electronic Retail Sales with ARIMA Forecast",
        ylab = "Retail Sales (in Millions of Dollars)", xlab = "Time (in years)")
abline(v = 2019.1, col = "black", lty = 2)
legend(x=1993.12, y=15000, lty=c(1,1,3), col=c("black", "red", "blue"),
       text.col=c("black", "red", "blue"), legend=c("Electronic Retail Sales", 
                                                    "ARIMA Forecast", "Confidence Interval"),text.font=1, cex=0.5)

ts.plot(df.forecast, col = c("black","red","blue","blue"),lty = c(1, 1, 2, 2),
        main = "ARIMA Forecast vs Retail Electronic Sales",
        xlab = "Time (in months) -- 2019",
        ylab = "Retail Sales (in millions of dollars)", ylim = c(5000, 12000))
legend(1, 11500, lty=c(1, 1, 2, 2), text.col=c("black","red","blue","blue"), 
       legend=c("Electronic Retail Sales (Test)", "ARIMA Forecast", "Confidence Interval - Low", 
                "Confidence Interval - High"),text.font=1, cex=0.5)

RMSE_ARIMA <- sqrt(mean((df.forecast$retails_electronic_test - df.forecast$exp.forecast.value)^2))
RMSE_ARIMA


#------------------------------------------------------------------#
####################################################################
########## VIII. Multiple regression with ARMA residuals ###########
####################################################################
#------------------------------------------------------------------#

retails_hobby_train = hobby %>% 
  window(end = c(2018, 12))

retails_furniture_train = furniture %>% 
  window(end = c(2018, 12))

retails_hobby_test = hobby %>% 
  window(start = 2019)

retails_furniture_test = furniture %>% 
  window(start = 2019)

x1 <- retails_hobby_train
x2 <- retails_furniture_train

x1.star <- log(x1)
x2.star <- log(x2)

### Section VIII.1 Causal model Fit

mlr.star <- lm(y.star ~ x1.star + x2.star)

mlr.star.residuals <- mlr.star$residuals

resmean <- mean(mlr.star.residuals)
ressd　<-　sd(mlr.star.residuals)
sdresiduals　<-　(mlr.star.residuals-resmean)/ressd # standardize the residuals

sdresiduals.ts <- ts(sdresiduals, start = start(retails_electronic_train), frequency = 12)
mlr.star.star.residuals = diff(diff(sdresiduals.ts, 1), 12)
plot(mlr.star.star.residuals, main = "Standardized Residuals of Multiple Regression Model",
     ylab = "Standardized Residuals", xlab = "Time (in years)")
abline(h = 0, col = "red")

par(mfrow = c(2, 1))
acf(mlr.star.star.residuals, lag.max = 50, main = "ACF - Standardized Residuals of Pre-Transformed Multiple Regression Model")
pacf(mlr.star.star.residuals, lag.max = 50, main = "PACF - Standardized Residuals of Pre-Transformed Multiple Regression Model")

# fitting arima model to residuals of multiple regression model
res_model <- arima(sdresiduals.ts, order = c(1, 1, 1), seas = list(order = c(1, 1, 1), 12))

res_model
par(mfrow = c(2, 1))
acf(resid(res_model), lag.max = 50, main = "ACF - Residuals of ARIMA Fit to Standarized Residuals")
pacf(resid(res_model), lag.max = 50, main = "PACF - Residuals of ARIMA Fit to Standarized Residuals")

Box.test(resid(res_model), lag = 12, type = "Ljung") # testing if white noise

library(nlme)
modelgls <- gls(y.star~x1.star+x2.star, correlation=corARMA(c(-0.1547, -0.6293), p = 1, q=1))
summary(modelgls)

# now checking if GLS model is good fit to data by analyzing residuals
par(mfrow=c(2,1))
acf(residuals(modelgls, type = "normalized"), lag.max = 50,
    main = "ACF - Residuals of GLS Model") 
pacf(residuals(modelgls, type = "normalized"), lag.max = 50,
     main = "PACF - Residuals of GLS Model")

# begin forecast onto test set
df_future <- data.frame(x1.star = log(retails_hobby_test), 
                        x2.star = log(retails_furniture_test))

gls_forecast <- predict(modelgls, df_future)

gls_forecast <- exp(gls_forecast)

ts.plot(cbind(retails_electronic_train,ts(gls_forecast[1:12], 
                                          start = start(retails_electronic_test), frequency = 12),
              retails_electronic_test), col = c("black", "red"),
        ylab = "Retail Sales (in Millions of Dollars)",
        xlab = "Time (in years)",
        main = "Electronic Retail Sales with GLS Forecast")
abline(v = 2019.1, col = "black", lty = 2)
legend(x=1993.12, y=15000, lty=c(1,1,3), 
       col=c("black", "red"), text.col=c("black", "red"), 
       legend=c("Electronic Retail Sales","GLS Forecast"), text.font=1, cex=0.5)

df = data.frame(retails_electronic_test, gls_forecast)
ts.plot(df, col = c("black", "red"), xlab = "Time (in months) -- 2019",
        ylab = "Retail Sales (in millions of Dollars)",
        main = "GLS Forecast vs Retail Electronic Sales")
legend(x=1, y=10700, lty=c(1,1,3),
       col=c("black", "red"),
       text.col=c("black", "red"), 
       legend=c("Electronic Retail Sales (Test)", "GLS Forecast"), text.font=1, cex=0.5)

RMSE_GLS <- sqrt(mean((retails_electronic_test - gls_forecast)^2))
RMSE_GLS


#------------------------------------------------------------------#
####################################################################
#################### IX. Vector autoregression #####################
####################################################################
#------------------------------------------------------------------#

# generate the multiple time series
mts.data <- cbind(retails_electronic_train, retails_furniture_train, retails_hobby_train)
colnames(mts.data) = c("electronic", "hobby", "furniture")

# multiple time series of pre-transformed data
mts.data_star = cbind(log(retails_electronic_train), sqrt(retails_hobby_train), log(retails_furniture_train))
colnames(mts.data_star) = c("electronic*", "hobby*", "furniture*")
plot(mts.data_star)

# view the acf of the multiple time series of pre-transformed data
acf(mts.data_star, lag.max = 50)

# multiple time series of differenced, pre-transformed data
mts.data_star_star = diff(diff(mts.data_star, 1), 12)
colnames(mts.data_star_star) = c("electronic**", "hobby**", "furniture**")
acf(mts.data_star_star, lag.max = 50, lwd=1.3) # approximately white noise


# Find the ccf
acf(mts.data_star_star, lag = 50)

library(vars)
# determined that p = 11 from the ccf plot
var.retail = VAR(mts.data, p = 11)

# extract significant coefficients using the summary function
summary(var.retail)

# determine if the roots are all less than 1
roots(var.retail, modulus = TRUE)

# constructing the impulse response function for three variables
irf_electronics <- irf(var.retail, impulse = "electronic", response = c("electronic", "hobby", "furniture"), n.ahead = 12)
irf_furniture <- irf(var.retail, impulse = "furniture", response = c("electronic", "hobby", "furniture"), n.ahead = 12)
irf_hobbies <- irf(var.retail, impulse = "hobby", response = c("electronic", "hobby", "furniture"), n.ahead = 12)

# plotting the imf for all three variables
plot(irf_electronics)
plot(irf_furniture)
plot(irf_hobbies)

# generate a prediction 
var.pred = predict(var.retail, n.ahead = 12, ci = 0.95)
var.pred.electronic = ts(var.pred$fcst$electronic[, 1], 
                         start = start(retails_electronic_test), frequency = 12)

# lower of the CI
var.pred.cilower = ts(var.pred$fcst$electronic[, 2], 
                      start = start(retails_electronic_test), frequency = 12)
# upper of the CI
var.pred.ciupper = ts(var.pred$fcst$electronic[, 3], 
                      start = start(retails_electronic_test), frequency = 12)

# this will be used to plot in the next step
var_df = cbind(retails_electronic_test, var.pred.electronic, 
               var.pred.cilower, var.pred.ciupper)

# close up of the testing set
ts.plot(var_df, 
        col = c("black", "red", "blue", "blue"), 
        lty = c(1, 1, 3, 3),
        main = "VAR of Just the Testing Data (p = 11)")

legend(x=2019.0, y=12000, lty=c(1,1,3),
       col=c("black", "red", "blue"),
       text.col=c("black", "red", "blue"), 
       legend=c("real_data", "VAR(p = 11)", "CI"),
       text.font=1, cex=1)

# overall time series
ts.plot(cbind(retails_electronic_train,
              ts(var_df[, c(2, 3, 4)], start = start(retails_electronic_test), 
                 frequency = 12),
              retails_electronic_test),
        col = c("black", "red", "blue", "blue"),
        lty = c(1, 1, 2, 2), main = "VAR Including Training Data (p = 11)")
abline(v = 2019.1, col = "green", lwd = 1.5)
legend(x=1993.12, y=15000, lty=c(1,1,3),
       col=c("black", "red", "blue"),
       text.col=c("black", "red", "blue"), 
       legend=c("real_data","VAR(p = 11)", "CI"),text.font=1, cex=1)

# RMSE
RMSE_VAR <- sqrt(mean((var_df[, 1] - var_df[, 2])^2))
RMSE_VAR


#------------------------------------------------------------------#
####################################################################
###################### X. Forecast Comparison ######################
####################################################################
#------------------------------------------------------------------#

forecast_comparison <- cbind(electronic_pred, exp(forecast$pred), 
                             gls_forecast, var.pred.electronic)
colnames(forecast_comparison) <- c("Exponential Smoothing", "ARIMA", 
                                   "AutoCorrelated Model","VAR")
ts.plot(forecast_comparison,
        col = c("red", "blue", "green", "brown"),
        lty = 5, lwd = 1.5, main = "Comparison of Forecast Methods",
        xlab = "Time (in years)", ylab = "Retail Electronic Sales (in millions of dollars)")

lines(retails_electronic_test, col = "black", lwd = 2)
legend(x = 2019.0, y = 12000, lty=c(1,2,2,2,2),
       col=c("black", "red", "blue", "green", "brown"),
       #text.col=c("black","red", "blue", "green", "brown"), 
       legend=c("Actual Values",
                "Exponential Smoothing", 
                "ARIMA", 
                "AutoCorrelated Model",
                "VAR"),text.font=1, cex=0.5)



#------------------------------------------------------------------#
####################################################################
########################## XI. Auto.ARIMA ##########################
####################################################################
#------------------------------------------------------------------#
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


df$month= factor(lubridate::month(df$date, label=TRUE), ordered=FALSE)  # month 

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
library(forecast)
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



ci.mean = ts(fcast1$mean, frequency = 12,start = c(2019,1))
ci.low = ts((fcast1$lower[,2]), frequency = 12,start = c(2019,1))
ci.high = ts((fcast1$upper[,2]), frequency = 12,start = c(2019,1))

df.forecast = data.frame(retails_electronic_test, exp(ci.mean), 
                         exp(ci.low), exp(ci.high)) #exp due to log transform

df.forecast
ts.plot(cbind(retails_electronic_train, exp(ci.low), exp(ci.mean),
              retails_electronic_test, exp(ci.high)), col=c("black", "blue", "red", "black" ,"blue"),
        lty = c(1,3,1,1,2), main="Electronic Retail Sales with Auto.ARIMA Forecast",
        ylab = "Retail Sales (in Millions of Dollars)", xlab = "Time (in years)")

abline(v = 2019.1, col = "black", lty = 2)
legend(x=1993.12, y=15000, lty=c(1,1,3), col=c("black", "red", "blue"),
       text.col=c("black", "red", "blue"), legend=c("Electronic Retail Sales", 
                                                    "Auto.ARIMA Forecast", "Confidence Interval"),text.font=1, cex=0.5)

ts.plot(df.forecast, col = c("black","red","blue","blue"),lty = c(1, 1, 2, 2),
        main = "Auto.ARIMA Forecast vs Retail Electronic Sales",
        xlab = "Time (in months) -- 2019",
        ylab = "Retail Sales (in millions of dollars)", ylim = c(5000, 12000))
legend(1, 11500, lty=c(1, 1, 2, 2), text.col=c("black","red","blue","blue"), 
       legend=c("Electronic Retail Sales (Test)", "Auto.ARIMA Forecast", "Confidence Interval - Low", 
                "Confidence Interval - High"),text.font=1, cex=0.5)

#------------------------------------------------------------------#
####################################################################
####################### XII. Machine Learning ######################
####################################################################
#------------------------------------------------------------------#

###### EDA

#   install.packages("TSstudio")
# library(TSstudio)
# As a package, TSstudio has its own functions. 


ts_info(electronic) # rapper for frequency(), start() and  end() combined
ts_plot(electronic, 
        title="Advance Retail Sales: Electronics and Appliance Stores", 
        Ytitle="Millions of Dollars",
        Xtitle="Year") 



##### decomposition
# The series components 
ts_decompose(electronic) #wrapper for decompose 

#Seasonal analysis
electronic_detrend=electronic-decompose(electronic)$trend


# the following is an enhanced wrapper for seasonal boxplot.
ts_seasonal(electronic_detrend,type="box") 


#The following is a wrapper for correlation analysis with acf and ccf 
# notice that the wrapper omits the lag 0 in the acf
# which the acf of a ts() object does not do. 
ts_cor(electronic) # red indicates seasonal and black indicates non-seasonal

## the following is a wrapper for lag plots 
ts_lags(electronic, lags=c(12,24,36))


##Select training data 
# See below a wrapper to create a training window of the data 
# and at the same time rename the variables to be ready for prophet. 
df=ts_to_prophet(window(electronic,start=c(2009,1))) # trend dies at 2008 and another one begins at January of 2009
names(df)=c("date","y")
head(df)
ts_plot(df, 
        title="Advance Retail Sales: Electronics and Appliance Stores (subset)", 
        Ytitle="Millions of Dollars",
        Xtitle="Year") 



######Feature engineering 
# create new features that will be used as inputs in the model 

#  install.packages("dplyr")
# library(dplyr)
# library(lubridate)

df <- df %>% mutate(month = factor(lubridate::month(date, label = TRUE), ordered = FALSE),
                    lag12 = lag(y, n = 12)) %>%
  filter(!is.na(lag12))

df <- df %>% mutate(month = factor(lubridate::month(date, label = TRUE), ordered = FALSE),
                    lag1 = lag(y, n = 1)) %>%
  filter(!is.na(lag1))

df$trend <- 1:nrow(df)
# We see from the time plots that in the subset that we selected, there is no strong trend.
# Moreover, seasonality plays a strong role.
# Thus, I will incorporate the trend component as a feature, but not trend squared
# Ultimately, this can prevent overfitting to the model.

# df$trend_sqr <- df$trend ^ 2 # trend squared

str(df)


## training, testing and model evaluation
h <- 12
train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]



## inputs for forecast 
forecast_df <- data.frame(date = seq.Date(from = max(df$date) + lubridate::month(1),
                                          length.out = h, by = "month"),
                          trend = seq(from = max(df$trend) + 1, length.out = h, by = 1))
# This above line creates a data frame called 'forecast_df' that contains two columns: 'date' and 'trend'. 
# The 'date' column contains a sequence of 'h' monthly dates starting from the next month after the 
# maximum date in the df$date column. The 'trend' column contains a sequence of numbers starting 
# from the maximum value in df$trend plus one, with increments of one.


# forecast_df$trend_sqr <- forecast_df$trend ^ 2

# to avoid conflict with the h2o `month` function use the "lubridate::month" to explicly call the month from the lubridate function 
forecast_df$month <- factor(lubridate::month(forecast_df$date, 
                                             label = TRUE), ordered= FALSE) 
# This line creates a new column in 'forecast_df' called 'month' that contains the 
# month name (as a factor) of each date in the 'date' column. The 'lubridate::month' function
# is used to extract the month from each date, and the 'label = TRUE' argument specifies that 
# the month names should be used as factor labels.

forecast_df$lag12 <- tail(df$y, 12)
# This line creates a new column in 'forecast_df' called 'lag12' that contains the 
# last 12 values of the 'y' column in the 'df' data frame.

forecast_df$lag1 <- tail(df$y, 1)
# This line creates a new column in 'forecast_df' called 'lag1' that contains the 
# last 1 values of the 'y' column in the 'df' data frame.


#################################################################
####################### Linear Regression #######################
#################################################################

##### benchmark the ML models with this model
lr <- lm(y ~ month + lag12 + lag1 + trend, data = train_df)
summary(lr)
test_df$yhat <- predict(lr, newdata = test_df)
mape_lr <- mean(abs(test_df$y - test_df$yhat) / test_df$y)
mape_lr


#### Prepare for ML 
# install.packages("h2o")
library(h2o)

h2o.init(max_mem_size = "16G")
train_h <- as.h2o(train_df)
test_h <- as.h2o(test_df)
forecast_h <- as.h2o(forecast_df)

forecast_h


#################################################################
######################### Random Forest #########################
#################################################################

###### Simple RF model using 500 trees and 5 folder CV 

### Training process. Will add stop criterion. Stopping metric 
## is RMSE, tolerance is 0.0001, stopping rounds set 
# to 10 (Krispin, 2019)

x=c("month", "lag12", "lag1", "trend")
y="y"

rf_md = h2o.randomForest(training_frame=train_h,
                         nfolds=5,
                         x=x, 
                         y=y, 
                         ntrees=500,
                         stopping_rounds=10,
                         stopping_metric="RMSE", 
                         score_each_iteration=TRUE,
                         stopping_tolerance=0.0001,
                         seed=1234)

## see the contribution of the mode inputs

h2o.varimp_plot(rf_md) # as expected, month and lag12 has strong contribution, trend not so much

## See model output 

rf_md@model$model_summary

rf_md

## learning process of the model as a function of the number of trees

library(plotly)
tree_score =rf_md@model$scoring_history$training_rmse

plot_ly(x=seq_along(tree_score), y=tree_score,
        type="scatter", mode="line") %>%
  layout(title="The trained Model Score History", 
         yaxis=list(title="RMSE"), 
         xaxis=list(title="Num. of Trees"))

## Model performance in test set. Forecasting performance 

test_h$pred_rf = h2o.predict(rf_md, test_h)
test_h$pred_rf  

## transfer h2o data frame to a data.frame object 

test_1= as.data.frame(test_h)

## Calculate the MAPE score of the RF model on the test partition

mape_rf = mean(abs(test_1$y -test_1$pred_rf)/test_1$y)
mape_rf  
# We have that the MAPE score of the RF model is higher than the benchmark regression model.


## Visualizing model performance 

plot_ly(data=test_1) %>%
  add_lines(x=~date, y=~y, name="Actual") %>%
  add_lines(x=~date, y=~yhat, name="Linear Regression", line=
              list(dash="dot")) %>%
  add_lines(x=~date, y=~pred_rf, name="RF", line=
              list(dash="dash")) %>% 
  layout(title="Total Retail Electronic Sales-Actual vs. Prediction (Random Forest)", 
         yaxis= list(title="Millions of Dollars"), 
         xaxis=list(title="Year"))

# Comparing the prediction values with the actual value, we have that the random forest over-forecasts for all the values.
# Furthermore, the linear regression model tends to follow the actual values well.


#################################################################
####################### Gradient Boosting #######################
#################################################################

### Train the GB model with the same input used in RF 

gbm_md =h2o.gbm( 
  training_frame = train_h,
  nfold=5, 
  x=x,
  y=y, 
  max_depth=20, 
  distribution="gaussian",
  ntrees=500, 
  learn_rate=0.1,
  score_each_iteration=TRUE 
)

## See model output 

gbm_md@model$model_summary

gbm_md

## How important are the model variables in the training (fitting)
h2o.varimp_plot(gbm_md)

## learning process of the model as a function of the number of trees

library(plotly)
tree_score =gbm_md@model$scoring_history$training_rmse

plot_ly(x=seq_along(tree_score), y=tree_score,
        type="scatter", mode="line") %>%
  layout(title="The Trained Model Score History", 
         yaxis=list(title="RMSE"), 
         xaxis=list(title="Num. of Trees"))

# test the model's performance on the testing set 

test_h$pred_gbm = h2o.predict(gbm_md, test_h)
test_1= as.data.frame(test_h)  

## calculate mape in the test set (of the forecast)
mape_gbm = mean(abs(test_1$y -test_1$pred_gbm)/test_1$y)
mape_gbm  
# The MAPE for the GB model is slightly greater than the benchmark regression model
# However, it shows that the GB model performs better than the RF model


## Visualizing model performance in the test set 

plot_ly(data=test_1) %>%
  add_lines(x=~date, y=~y, name="Actual") %>%
  add_lines(x=~date, y=~yhat, name="Linear Regression", line=
              list(dash="dot")) %>%
  add_lines(x=~date, y=~pred_gbm, name="GBM", line=
              list(dash="dash")) %>% 
  layout(title="Total Retail Electronic Sales-Actual vs. Prediction (GBM)", 
         yaxis= list(title="Millions of Dollars"), 
         xaxis=list(title="Month"))
# Similar to the RF model, the GB model over predicts the actual testing data
# However, notice that the predictions became close to the actual value
# which goes along with the fact that the MAPE value for GB model is less than RF model


#################################################################
############################ Prophet ############################
#################################################################

# install.packages('prophet')
library(prophet)

# ?ts_to_prophet
df=ts_to_prophet(window(electronic,start=c(2009,1))) # trend dies at 2008 and another one begins at January of 2009
names(df)=c("ds","y") # make sure to have "ds" and "y"
head(df)

h <- 12
train_df <- df[1:(nrow(df) - h), ]
test_df <- df[(nrow(df) - h + 1):nrow(df), ]

#fit the model by instantiating a new Prophet object
prophet_model <- prophet(train_df)

# get a suitable dataframe that extends into the future a specified number of days
future <- make_future_dataframe(prophet_model, freq = "month", periods = 12) # without freq = "month" we will be looking at days
future[121:132,]

# assign each row in future a predicted value which names yhat
forecast <- predict(prophet_model, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
# plot the forecast
plot(prophet_model, forecast)
prophet_plot_components(prophet_model, forecast)

# making data frame that will be used for plotting
test_2 = test_df
test_2["yhat"] = test_1["yhat"] # linear regression values
prophet_forecast_val = tail(forecast$yhat, 12)
test_2["pred_proph"] = prophet_forecast_val # prophet forecast
test_2 = as.data.frame(test_2) 

# plotting against actual and linear regression
plot_ly(data=test_2) %>%
  add_lines(x=~ds, y=~y, name="Actual") %>%
  add_lines(x=~ds, y=~yhat, name="Linear Regression", line=
              list(dash="dot")) %>%
  add_lines(x=~ds, y=~pred_proph, name="Prophet", line=
              list(dash="dash")) %>% 
  layout(title="Total Retail Sales-Actual vs. Prediction (Prophet)", 
         yaxis= list(title="Millions of Dollars"), 
         xaxis=list(title="Month"))

mape_prophet = mean(abs(test_2$y - test_2$pred_proph)/test_2$y)
mape_prophet  

# Notice that prophet does a very good job.
# The Green dashed line follows the actual value very closely
# Further, notice that the MAPE for the prophet forecasting is very small
# although not as smaller than the linear regression.




#################################################################
########### Forecast Comparison and Final Conclusion ############
#################################################################

##### Mean Absolute Percentage Error (MAPE)

# linear regression MAPE
mape_lr

# random forest MAPE
mape_rf

# gradient boosting MAPE
mape_gbm

# prophet MAPE
mape_prophet


##### Root Mean Square Error (RMSE)

# linear regression RMSE
RMSE_lr <- sqrt(mean((test_1$y - test_1$yhat)^2))
RMSE_lr

# random forest RMSE
RMSE_rf <- sqrt(mean((test_1$y - test_1$pred_rf)^2))
RMSE_rf

# gradient boosting RMSE
RMSE_gbm <- sqrt(mean((test_1$y - test_1$pred_gbm)^2))
RMSE_gbm

# prophet RMSE
RMSE_prophet <- sqrt(mean((test_2$y - test_2$pred_proph)^2))
RMSE_prophet

# Generating a table to compare errors

error_table <- rbind(c(mape_lr, RMSE_lr),
                     c(mape_rf, RMSE_rf),
                     c(mape_gbm, RMSE_gbm),
                     c(mape_prophet, RMSE_prophet))
colnames(error_table) <- c("MAPE", "RMSE")
rownames(error_table) <- c("Linear Regression (Benchmark)", "Random Forest", "Gradient Boosting", "Prophet")
error_table

