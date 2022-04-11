########################
### Chapter 4: ARCH ####
########################

install.packages('quantmod')
install.packages("tseries")
install.packages("fGarch")

# -----------------------------------------------------------------------------
# Figure 4.1: FTSE 100 price and returns
require('quantmod')
getSymbols('MSFT', src='yahoo')
p <- ts(MSFT$MSFT.Close[800:1562]) # 8th March 2010 to 18th March 2013 (n=763)
r <- diff(log(p))[-1]
par(mfrow=c(2,1)); tsplot(p, ylab="Close Price", col="red"); tsplot(r, ylab="Returns", col="red")

# -----------------------------------------------------------------------------
# Figure 4.2: Returns ACF and PACF
par(mfrow=c(2,1)); acf(r); pacf(r, ylab="PACF")

# -----------------------------------------------------------------------------
# Figure 4.3: Squared returns ACF and PACF
par(mfrow=c(2,1)); acf(r^2); pacf(r^2, ylab="PACF")

# -----------------------------------------------------------------------------
# Section 4.4: Fitted GARCH model
library(tseries)
summary(ftse.garch <- garch(r, order=c(1,1), trace=F))

# -----------------------------------------------------------------------------
# Figure 4.4: Predicted volatilities
vol <- 0
vol[1] <- 0
for(t in 2:998)
  vol[t] <- ftse.garch$coef[1] + ftse.garch$coef[2]*(r[t-1])^2 + ftse.garch$coef[3]*vol[t-1]
vol <- sqrt(vol)
par(mfrow=c(1,1))
ts.plot(r, vol, -vol, col=c("red", "blue", "blue"))
legend('topleft', col=c("red", "blue"), lty=1, lwd = 2, 
       legend=c("Returns", "Predicted volatility"), bty="n")

# -----------------------------------------------------------------------------
# Figure 4.5: Residual ACF
ftse.res <- ftse.garch$residuals[-1]
par(mfrow=c(1,1)); acf(ftse.res-mean(ftse.res))

# -----------------------------------------------------------------------------
# Figure 4.5: Residual Q-Q plot
par(mfrow=c(1,1)); qqnorm(ftse.res); qqline(ftse.res)

