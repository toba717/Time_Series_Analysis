
### I. Introduction
library(Quandl)
library(dygraphs)
Quandl.api_key("") 

retail_electronic = Quandl(code="FRED/RSEASN",type="ts", collapse="monthly", meta=TRUE)
# Do not include 2020 and 2021 years in our data
electronic = window(retail_electronic, end = c(2019, 12))
# > 200 observations & start: 1992 1

retail_hobby = Quandl(code = "FRED/RSSGHBMSN", type = "ts", collapse = "monthly", meta = TRUE)
# Do not include 2020 and 2021 years in our data
hobby = window(retail_hobby, end = c(2019, 12))
# > 200 observations & start: 1992 1

retail_furniture = Quandl(code = "FRED/RSFHFSN", type = "ts", collapse = "monthly", meta = TRUE)
# Do not include 2020 and 2021 years in our data
furniture = window(retail_furniture, end = c(2019, 12))
# > 200 observations & start: 1992 1

retail_sales <- cbind(electronic, hobby, furniture)
dygraph(retail_sales, main = "Retail Sales") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(3, "Set2"))

retails_electronic_train = electronic %>% 
  window(end = c(2018, 12))

retails_elecronic_test = electronic %>% 
  window(start = 2019)


### II. Components Features of the dependent variable
plot(retails_electronic_train)
add_decomp_plot <- (decompose(retails_electronic_train, type='add')) 
mult_decomp_plot <- decompose(retails_electronic_train, type = "mult")
plot(add_decomp_plot)
plot(mult_decomp_plot)
add_decomp_plot$seasonal
mult_decomp_plot$seasonal

boxplot_elec <- boxplot(retails_electronic_train ~ cycle(retails_electronic_train))

## III. 
acf(retails_electronic_train)

retails_electronic_train_log = log(retails_electronic_train)

retails_electronic_train_quadratic = retails_electronic_train^2

retails_electronic_train_squared = sqrt(retails_electronic_train)

par(mfrow = c(3, 1))
plot(retails_electronic_train_log, main = "log") 
plot(retails_electronic_train_quadratic, main = "quadratic")
plot(retails_electronic_train_squared, main = "squared")

log_data = diff(retails_electronic_train_log)
# plot(log_data) # now it is a mean of zero and has a constant variance

par(mfrow=c(1, 2))
acf(log_data, main = 'ACF')
pacf(log_data, main = 'PACF')

seasonal_diff = diff(retails_electronic_train_log, lag = 12)
plot(log_data) # now it is a mean of zero and has a constant variance

par(mfrow=c(1, 2))
acf(log_data, main = 'reular')
acf(seasonal_diff, main = 'seasonal')

plot(seasonal_diff)

## IV.
electronic_smooth <- HoltWinters(retails_electronic_train, seasonal = "multiplicative")
plot(electronic_smooth, main = "Exponential Smoothing Model of Electronic Retail Sales", xlab = "Year", ylab = "Retail Sales (in Millions of Dollars")

electronic_pred <- predict(electronic_smooth, n.ahead = 12)
electronic_pred

plot(electronic_smooth, electronic_pred, main = "Exponential Smoothing of Electronic Retail Sales With Forecasting",
     xlab = "Year", ylab = "Retail Sales (in Millions of Dollars)")
lines(retails_elecronic_test, col = "blue", lty = 2)

par(mfrow = c(1, 2))
plot(electronic_pred, main = "Actual (black) Vs Predicted\n (red) Retail Sales in 2019", xlab = "Time (in years)", ylab = "Retail Sales (in millions of dollars")
lines(retails_elecronic_test, col = "red", lty = 2)

residual = as.numeric(retails_elecronic_test) - as.numeric(electronic_pred)
plot(residual, main = "Residuals of Predicted\n Retail Sales in 2019", xlab = "Time (in years)", ylab = "Residuals (in millions of dollars")
abline(h = 0, col = "red")

## V.
df = data.frame(as.numeric(time(retails_electronic_train)))

df$values = as.numeric(retails_electronic_train)

colnames(df)[1] <- "retail_electronics_train"

quad_model <- lm(values ~ poly(retail_electronics_train, 2), data = df)

summary(quad_model)

testTimeRE <- data.frame(as.numeric(time(retails_elecronic_test)))
colnames(testTimeRE)[1] <- "retail_electronics_train"
predictions <- predict(quad_model, newdata=testTimeRE)
seasonals <- as.numeric(mult_decomp_plot$seasonal)[1:12]
total_predict <- predictions * seasonals
quad_predict <- ts(total_predict)

quad_predict



electronic = window(retail_electronic, end = c(2019, 12))

plot(electronic)
elec_log <- log(electronic)
plot(elec_log)

elec_log_diff  <- diff(elec_log) # normal
plot(elec_log_diff)

elec_log_diffdiff <- diff(elec_log_diff, lag = 12) # seasonal
plot(elec_log_diffdiff)


par(mfrow = c(1,2))
acf(elec_log_diffdiff)
pacf(elec_log_diffdiff)

# generating the model
model1 <- arima(retails_electronic_train_log, order = c(0,1,1), seas = list(order = c(0,1, 1), 12))
model1


model2 <- arima(retails_electronic_train_log, order = c(0,1,1), seas = list(order = c(0,1, 2), 12))
model2


forecast = predict(model2, n.ahead = 12)
forecast.value = ts((forecast$pred), start = start(retails_elecronic_test), freq =12)

ts.plot(data.frame(retails_elecronic_test, exp(forecast.value)), col = c("blue", "red"))




############################################################################
##HW 2
############################################################################

### Load libraries and API Key
library(Quandl)
library(dygraphs)
Quandl.api_key("uSPQnqK5PuPzTydvoB_R") 

### Load Datasets
retail_electronic = Quandl(code="FRED/RSEASN",type="ts", collapse="monthly", meta=TRUE)
# Do not include 2020 and 2021 years in our data
electronic = window(retail_electronic, end = c(2019, 12))
# > 200 observations & start: 1992 1

### Examine Data
retail_electronic # not too long so can view whole dataset
head(retail_electronic)
tail(retail_electronic)
class(retail_electronic)
str(retail_electronic)
start(retail_electronic)
end(retail_electronic)
frequency(retail_electronic)
length(retail_electronic)

### Creating a training and testing set
retails_electronic_train = window(electronic, start = start(electronic), end = c(2019,12), freq = 12)
retails_electronic_test = window(electronic, start = c(2019, 1), freq = 12)

### Call the raw training dependent variable y
y = retails_electronic_train

### We look at Time Plot
plot.ts(y, main = "Retail Sales for Electronics", ylab = "Retail Sales (in Millions of Dollars)")
### Seasonal Box Plot
boxplot(y ~ cycle(y), ylab = "Retail Sales (in Millions of Dollars)", xlab = "Month")


### To see if the time series has changing variance, get a hint through decomposition
plot(decompose(y))
plot(decompose(y, type = "mult"))


##### Step 1 is to pre-transform the data
log.ts = ts(log(y), frequency = 12, start(y))
sqr.ts = ts(sqrt(y), frequency = 12, start(y))
qrt.ts = ts((y)^0.25, frequency = 12, start(y))

### Compare data with pre-differenced and pre-transformed data
par(mfrow = c(2,2))
plot.ts(y, type = "l", main = "Time Series Plot of Retail Sales for Electronics")
plot.ts(log.ts, type = "l", main = "Time Series Plot of log(Retail Sales) for Electronics")
plot.ts(sqr.ts, type = "l", main = "Time Series Plot of sqrt(Retail Sales) for Electronics")
plot.ts(qrt.ts, type = "l", main = "Time Series Plot of qrt(Retail Sales) for Electronics")
dev.off()

### Log transformation performs the best, so we will call this y*
ystar = log.ts


# I don't think we need this
###########################################################################################################




retails_furniture_train = furniture %>% window(end = c(2018, 12))
retails_retails_train_log = log(retails_furniture_train)
plot(sqrt(retails_furniture_train))
plot(log(retails_furniture_train))
plot((retails_furniture_train))

retails_hobbies_train = hobby %>% window(end = c(2018, 12))
retails_hobbies_train_log = log(retails_hobbies_train)
plot(sqrt(retails_hobbies_train))
plot(log(retails_hobbies_train))
plot((retails_hobbies_train))

# retails_elecronic_test = electronic %>% window(start = 2019)


par(mfrow = c(1,2))
model1 = lm(retails_electronic_train_log ~ retails_retails_train_log+ retails_hobbies_train_log)
summary(model1) # find r squared value
acf(model1$residual, lag = 50)
pacf(model1$residual, lag = 50) #pacf cuts off, so use AR

resmodel1=arima(residuals(model1), order=c(1,0,0), include.mean=F)
acf(resmodel1$residuals)


resmodel2=arima(residuals(model1), order=c(2,0,0), include.mean=F)
acf(resmodel2$residuals)

resmodel3=arima(residuals(model1), order=c(12,0,0), include.mean=F)
acf(resmodel3$residuals)


# install.packages("nlme") 
library(nlme)
# remove.packages("nlme")

# this takes a long time to run
models = gls(retails_electronic_train_log ~ retails_retails_train_log+ retails_hobbies_train_log, correlation = corARMA(c(0.3218, 0.0678,  0.0496,  0.0143,  -0.0014,  -0.2160,  0.1706,  0.0265,  0.0315,  -0.0466,  -0.0049,  0.5800), p = 12))

dev.off()

plot(y = residuals(models, type = "normalized"), x = as.vector(time(retails_electronic_train_log)), type = "l")
abline(h = 0)

acf(ts(residuals(models, type = "normalized")), lag = 50)


resmodel3=arima(residuals(model1), order=c(12,0,0), include.mean=F)
acf(resmodel3$residuals)


###################################################
# Trial 2

# this takes a long time to run
resmodel4=arima(residuals(model1), order=c(12,0,12), include.mean=F)
acf(resmodel4$residuals, lag = 50)
models1 = gls(retails_electronic_train_log ~ retails_retails_train_log+ retails_hobbies_train_log, correlation = corARMA(c(-0.0284,  0.0057,  -0.0253,  -0.0046,  -0.0022,  -0.0285,  -0.0170,  -0.0068,  0.0096,  -0.0131,  0.030,  0.9461, 0.5429,  0.4512,  0.4660,  0.5050,  0.5435,  0.4819,  0.4544,  0.4663,  0.4525,  0.4159,  0.3656,  -0.1197), p = 12, q = 12))

dev.off()

plot(y = residuals(models1, type = "normalized"), x = as.vector(time(retails_electronic_train_log)), type = "l")
abline(h = 0)

acf(ts(residuals(models1, type = "normalized")), lag = 50)


###################################################

###########################################################################################################
