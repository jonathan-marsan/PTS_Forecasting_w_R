library(forecast)

Amtrak.data <- read.csv("Amtrak data.csv")

# ts() creates a time series object
ridership.ts <- ts(Amtrak.data$Ridership,
                   start = c(1991,1),
                   end = c(2004, 3),
                   freq = 12)

# tslm() adds a linear regression to a time series object, output is "lm" object
# In this case, we apply a quadratic trend using "trend + I(trend^2)"
# y is ridership.ts values
# x is quadratic function usingtime period (1,2,3...max(time period)) and its square
# I() treats square of trend "as is", i.e. prevents use as formula operators
# and forces "+", "-", "*" and "^" to be treated as simple arithmetical operators
ridership.lm <- tslm(ridership.ts ~ trend + I(trend^2))

# par() used to modify graphical parameter settings
# In this case, it divides screen in upper and lower sections to fit two graphs
par(mfrow = c(1, 1))

# Figure 2-6
plot(ridership.ts, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300),
     bty = "l")
lines(ridership.lm$fitted, lwd = 5) #  lwd is line width

# Use the stats::window to get subset of a class 'ts' object
ridership.ts.zoom <- window(ridership.ts, start = c(1997, 1), end = c(2000, 12))
# Compute trend
ridership.lm.zoom <- tslm(ridership.ts.zoom ~ trend + I(trend^2))
plot(ridership.ts.zoom, xlab = "Time", ylab = "Ridership", ylim = c(1300, 2300),
     bty = "l")
lines(ridership.lm.zoom$fitted, lwd = 2)