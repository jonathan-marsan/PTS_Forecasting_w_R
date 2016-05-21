
# Problems 2.6
# No. 1 Impact of Sept 11 on Air Travel in the US
library(xlsx)
library(forecast)
passengerMovement <- read.xlsx("data/Sept11Travel.xls",
                               sheetIndex = 1)
airlineRevenue <- ts(passengerMovement$Air.RPM..000s.,
                     start = c(1990,1),
                     end = c(2004,3),
                     freq = 12)
airlineRevenue.lm <- tslm(airlineRevenue ~ trend + I(trend^2))
railMiles <- ts(passengerMovement$Rail.PM,
                start = c(1990,1),
                end = c(2004,3),
                freq = 12)
railMiles.lm <- tslm(railMiles ~ trend + I(trend^2))
autoMiles <- ts(passengerMovement$VMT..billions.,
                start = c(1990,1),
                end = c(2004,3),
                freq = 12)
autoMiles.lm <- tslm(autoMiles ~ trend + I(trend^2))

# Plot above with trend lines
par(mfrow = c(3,1))
plot(airlineRevenue, xlab = "Time", ylab = "Airline Revenue", bty = "l")
lines(airlineRevenue.lm$fitted,lwd=2)
plot(railMiles, xlab = "Time", ylab = "Rail Miles", bty = "l")
lines(railMiles.lm$fitted,lwd=2)
plot(autoMiles, xlab = "Time", ylab = "Auto Miles", bty = "l")
lines(autoMiles.lm$fitted,lwd=2)

