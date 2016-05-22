
# Problems 2.6
######################################
# No. 1 Impact of Sept 11 on Air Travel in the US
library(xlsx)
library(forecast)
library(dplyr)
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
######################################
# No 2 Forecasting Department Store Sales
dptStoreSales <- read.xlsx("data/DepartmentStoreSales.xls",
                           sheetIndex = 1)
dptStoreSales.ts <- ts(dptStoreSales$Sales,
                       start = c(1,1),
                       end = c(6,4),
                       freq = 4)
dptStoreSales.lm <- tslm(dptStoreSales.ts ~ trend + I(trend^2))
par(mfrow = c(1,1))
plot(dptStoreSales.ts, xlab = "Quarters", ylab = "Sales", bty = "l")
lines(dptStoreSales.lm$fitted,lwd=2)
abline(v = c(1:6), col = "gray60")
# Plot description: Upward exponential trend with additive seasonality
######################################
# No 3 Shipments of Household Appliances
applianceShipments <- read.xlsx("data/ApplianceShipments.xls",
                                sheetIndex = 1)
applianceShipments <- cbind(Shipments = applianceShipments$Shipments,
                            data.frame(
                              do.call('rbind',
                                  strsplit(as.character(applianceShipments$Quarter),
                                           '-',fixed = TRUE))))
applianceShipments <- applianceShipments %>%
                        arrange(X2,X1)
applianceShipments.ts <- ts(applianceShipments$Shipments,
                            start = c(1985,1), end = c(1989,4),
                            frequency = 4)
applianceShipments.lm <- tslm(applianceShipments.ts ~ trend)
plot(applianceShipments.ts, xlab = "Year", ylab = "Appliance shipments (in million USD)",
     bty = "l")
abline(v = c(1985:1989), col = "gray60")
lines(applianceShipments.lm$fitted, lwd = 2)