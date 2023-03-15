data = ts(read.table("http://www.stat.ucla.edu/~jsanchez/data/ApprovActiv.txt", header = TRUE, sep = '\t'), start = c(1996, 1), freq = 4)
data
ts.plot(data,lty = c(1,3), main = "Approvals and Construction Activity")
legend(2002,7500, c("Approvals", "Activity"), lty = c(1,3))
acf(data)

data1 = diff(data,1)
ts.plot(data1, lty = c(1,3))
legend(2002,-2000, c("Approvals", "Activity"), lty = c(1,3))
acf(data1)

# Mod(polyroot(c(1,-1.2, 0.27)))
