######################
# Time Series for Data Science
# Takao Oba
# "Discussion 1 Replication" 
########################

##################EXERCISE #################
# Run the whole program given with your Quandl key 
## and then remove key and then
## You finish the program by doing 
## the same we did in sections III and IV 
## but for d

## Submit the whole program: what I went through+ your code. 
## label the parts III and IV for d. 


install.packages("Quandl") # installing Quandl package
# ?Quandl() # retrieving Quandl information
library(Quandl) # load Quandl package
Quandl.api_key("")


# I
d = Quandl(code="FRED/CGDD25O", 
           type="ts",
           collapse="monthly",
           order="asc",
           end_date="2022-01-01",
           meta=TRUE)
# where the CGDD25O is the code for the doctor's or PhD's

# II
d # check the inputted data
class(d) # check if it is time series
str(d) # checking the metadata

install.packages("dygraphs")
library(dygraphs)
# interactive plot
dygraph(d,ylab="Unemployment, NSA (%)", 
        main="Cyclical, perhaps seasonality, \n 
                            unusual COVID period")%>% dyRangeSelector
# non-interactive plot
plot(d)
plot(d, ylim = c(0,12), ylab = "unemployment (%)")


# III
start(d) # start date of time series data
end(d) # end date of time series data
frequency(d) # 12 indicates months
plot(d, ylab = "Unemploment, Doctor's 25+, NSA (%)", main = "Cyclical, perhaps seasonality \n unusual COVID period")

# always want the raw data, not the aggregated data
# making data into annual
d.ag = aggregate(d, FUN = mean) # choice of FUN is either mean or total
frequency(d.ag) # the frequency has changed to one from above code
plot(d.ag)

# aggregate to quarterly
d.agq = aggregate(d, FUN = mean, nfrequency = 4)
frequency(d.agq) # the frequency has changed to four from above code
plot(d.agq)


# IV
time = time(d); time # extracting the time
cycle = cycle(d); cycle # extracting factor for month

# plotting a boxplot to view seasonality feature
boxplot(d~cycle, 
        xlab="month", 
        ylab="Unemployment, Doctors 25 +, NSA (%)",
        main="Very little seasonality, highest \n
                median in August, COVID outliers obvious") # not a lot of seasonality

# decomposition of the time series
decompose(d)
plot(decompose(d)) # notice to the right the four plots of "observed" "trend" "seasonal" "random"

# we can extract specific components from the decomposition
decompd = decompose(d)
trend.d = decompd$trend; class(trend.d)
# plot(trend.d)
seasonal.d = decompd$seasonal; class(seasonal.d)
# plot(seasonal.d)
random.d = decompd$random; class(random.d)
# plot(random.d)

window(d, start = c(2000,1), end = c(2018, 1))

#################################
