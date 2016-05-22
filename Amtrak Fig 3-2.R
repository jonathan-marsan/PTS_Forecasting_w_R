library(forecast)
Amtrak.data <- read.csv("Amtrak data.csv")
ridership.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)

stepsAhead <- 36
# Define training period
nTrain <- length(ridership.ts) - stepsAhead
# Use window() to create training subset
train.ts <- window(ridership.ts, start = c(1991, 1), end = c(1991, nTrain))
ridership.lm <-  tslm(train.ts ~ trend + I(trend^2))
# Use forecast() to make prediction based on training set (train.ts)
ridership.lm.pred <- forecast(ridership.lm, h = stepsAhead, level = 0)

# Figure 3-2
plot(ridership.lm.pred, ylim = c(1300, 2600),  ylab = "Ridership", xlab = "Time", bty = "l", xaxt = "n", xlim = c(1991,2006.25), main = "", flty = 2)
axis(1, at = seq(1991, 2006, 1), labels = format(seq(1991, 2006, 1))) 
lines(ridership.lm$fitted, lwd = 2)
lines(valid.ts)
lines(c(2004.25 - 3, 2004.25 - 3), c(0, 3500)) 
lines(c(2004.25, 2004.25), c(0, 3500))
text(1996.25, 2500, "Training")
text(2002.75, 2500, "Validation")
text(2005.25, 2500, "Future")
arrows(2004 - 3, 2450, 1991.25, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5 - 3, 2450, 2004, 2450, code = 3, length = 0.1, lwd = 1,angle = 30)
arrows(2004.5, 2450, 2006, 2450, code = 3, length = 0.1, lwd = 1, angle = 30)

# Create validation set
valid.ts <- window(ridership.ts, start = c(1991, nTrain + 1), end = c(1991, nTrain + stepsAhead))
# Check Goodness-of-fit with accuracy() to get ME, MAE, etc.
accuracy(ridership.lm.pred$mean, valid.ts)
# Check prediction accuracy of validation set only
accuracy(tail(ridership.lm.pred$fitted,36),
         tail(valid.ts,36))


