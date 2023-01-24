frequency(AirPassengers)


jj = JohnsonJohnson
frequency(jj)

AP=AirPassengers

new.decomp=decompose(AP, type="mult")  # we try multiplicative decomposition now

plot(new.decomp)

trend=new.decomp$trend

seasonal=new.decomp$seasonal

random=new.decomp$random

plot(trend,main="Trend",ylab="average")

plot(seasonal,main="Seasonal component of AP",ylab="AP-Trend")

plot(random,main="Random component of AP", ylab="AP-trend-seasonal")


trend
seasonal
random
171.2500*1.2265555*0.9474041




nottem
new.decomp=decompose(nottem, type="additive")
new.decomp$seasonal



install.packages("Quandl")
?Quandl() # get information
library(Quandl)
Quandl.api_key("uSPQnqK5PuPzTydvoB_R")



# Access Missouri's unemployment rate from 
#  FRED's database

ts2= Quandl(code="FRED/LNU03032241",
            type="ts",
            collapse="monthly",
            order="asc",
            end_date="2022-12-31",
            meta=TRUE)

plot(ts2, main = "Unemployment Level \n of Leisure, Hospitaliy, \nPrivate Wage and Salary Workers", ylab = "Thousands of Persons" )
dygraph(ts2,main="Unemployment Level \n of Leisure, Hospitaliy, \nPrivate Wage and Salary Workers",
        ylab="Thousands of Persons")



ts2 # not too long so can view whole dataset
head(ts2)
tail(ts2)
class(ts2)
str(ts2)
start(ts2)
end(ts2)
ts2  ## View only because it is not too big.


#### Access California's unemployment 
### rate from FRED's database


ts3= Quandl(code="FRED/CAUR",
            type="ts",
            collapse="monthly",
            order="asc",
            end_date="2020-07-31",
            meta=TRUE)
head(ts3)
tail(ts3)
class(ts3)
str(ts3)

start(ts3)
end(ts3)
ts3  ## View only because it is not too big. 

#### Access the UK unemployment rate from 
## the UK Office of National Statistics 
## database 

ts4= Quandl(code="UKONS/MGSX_M",
            type="ts",
            collapse="monthly",
            order="asc",
            end_date="2020-07-31",
            meta=TRUE)
head(ts4)
tail(ts4)
class(ts4)
str(ts4)
start(ts4)
end(ts4)
ts4 ## View only because it is not too big.


##### Prepare for graph of the three series in one plot

#png("demoquandlplots.png", units="in", width=8.5, height=8, res=1200)
par(
  mfrow=c(1,1),
  font.axis=2,
  mar=c(5,5,5,5),
  font.main=2,
  font.lab=2
)

#### Graph 

plot.ts(ts2,
        ylab="Unemployment rate",
        main="FRED and UKONS unemployment \n data from Quandl's web API",
        ylim=c(1,16),lwd=1.5,cex=0.5,lty=1)
lines(ts3, col="red",lwd=1.5,cex=0.5,lty=2)
lines(ts4, col="blue",lwd=1.5,cex=0.5,lty=6)



legend("topleft", legend = c("MO", "CA", "UK "),
       col = c("black", "red", "blue"),lty=c(1,2,6))

#dev.off()


#### Sometimes, a dygraph helps you see details.

###Using package dygraph to do an interactive graph.

install.packages("dygraphs")
library(dygraphs)
dygraph(ts2,main="MO Unemployment rate",
        ylab="Percentage") %>% dyRangeSelector






osvisit=read.table("http://www.stat.ucla.edu/~jsanchez/timeseriestimedata/osvisit.dat", header=F)
osvisit = ts(osvisit, start = c(1977, 1), freq = 12)

boxplot(osvisit~cycle(osvisit))


