######################
# Time Series for Data Science
# Takao Oba
# "Practice with ts() data sets" 
########################


# dataset that will be using
Nile

# what is the data set about
?Nile

# start
start(Nile)

# end
end(Nile)

# frequency
frequency(Nile)

# length
length(Nile)

# number of NA
sum(is.na(Nile))

# Create window of the data
window(Nile, start = c(1900,1), end = c(1950, 1))

# time function
time(Nile)

# cycle function
cycle(Nile)

# select two time series
# nhtemp has a frequency of 1
ts.intersect(Nile, nhtemp)


CBE <- read.table("http://www.stat.ucla.edu/~jsanchez/timeseriestimedata/cbe.dat", header = T)
Elec.ts <- ts(CBE[, 3], start = 1958, freq = 12)

Plot.ts = plot.ts(Elec.ts)

