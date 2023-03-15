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


# dependent
retails_electronic_train = electronic %>% 
  window(end = c(2018, 12))

retails_elecronic_test = electronic %>% 
  window(start = 2019)

# independent
retails_furniture_train = furniture %>% 
  window(end = c(2018, 12))

retails_furniture_test = furniture %>% 
  window(start = 2019)

retails_hobby_train = hobby %>% 
  window(end = c(2018, 12))

retails_hobby_test = hobby %>% 
  window(start = 2019)

plot(log(retails_electronic_train))
plot(log(retails_furniture_train))
plot(sqrt(retails_hobby_train))
plot(log(retails_hobby_train))
retails_electronic_train_log = (log(retails_electronic_train))
retails_furniture_train_log = (log(retails_furniture_train))
retails_hobbies_train_sqrt = (sqrt(retails_hobby_train))


plot(retails_hobbies_train_sqrt)

electronic_diff = diff(retails_electronic_train_log)
electronic_diffdiff = diff(electronic_diff, lag = 12)
acf(electronic_diffdiff, lag = 50)
plot(electronic_diff)

furniture_diff = diff(retails_furniture_train_log, lag = 1, diff = 2)
# furniture_diff = diff(furniture_diff)
furniture_diffdiff = diff(furniture_diff, lag =12, diff = 2)
acf(furniture_diffdiff, lag =50)
pacf(furniture_diffdiff, lag =50)
plot(retails_furniture_train_log)

dev.off()

hobbies_diff = diff(retails_hobbies_train_sqrt, lag = 1, diff = 2)
hobbies_diffdiff = diff(hobbies_diff, lag =12, diff = 2)
plot(hobbies_diffdiff)
acf(hobbies_diffdiff, lag =50)
pacf(hobbies_diffdiff, lag = 50)

electronic_diffdiff_scaled = window(electronic_diffdiff, start = c(1994,3))


mymts = cbind(electronic_diffdiff_scaled, furniture_diffdiff, hobbies_diffdiff)

class(mymts)

par(mfrow = c(1,2))
ccf(mymts[,1], mymts[,2])
ccf(mymts[,1], mymts[,3])



par(mfrow = c(1,1),
    font.axis = 2,
    font.main = 2,
    font.lab = 2,
    mar = c(5, 5, 4, 4))      

elec_furn = mymts[,c(1,2)]
colnames(elec_furn) = c("electronics", "furniture")

## SACF and SCCF of the mts object "data" reveals nonstationarity in at least activity    
acf(elec_furn,lwd=1.5,cex=1.5,lag=50)

elec_hobby = mymts[,c(1,3)]
colnames(elec_hobby) = c("electronics", "hobbies")
acf(elec_hobby,lwd=1.5,cex=1.5,lag=50)



########################################################################################################
