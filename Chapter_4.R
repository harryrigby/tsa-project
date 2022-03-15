##################################
# ARCH(p) MODEL FOR STOCK RETURNS
##################################

### Required Packages ###
install.packages('quantmod')
install.packages("tseries")
install.packages("fGarch")

### Sourcing Data ###
require('quantmod')
getSymbols('MSFT', src='yahoo')
p <- ts(MSFT$MSFT.Close[800:1562]) # 8th March 2010 to 18th March 2013 (n=763)
r <- diff(log(p))[-1]

### Series Plots and Correlation Plots of Returns (to show data is uncorrelated) ###
par(mfrow=c(2,1))
tsplot(p, ylab="Close Price", col="red"); tsplot(r, ylab="Returns", col="red")
acf(r); pacf(r, ylab="PACF")

### Series Plots and Correlation Plots of Absolute and Squared Returns (to show data is NOT iid) ###
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
tsplot(r^2, col="red"); acf(r^2); pacf(r^2, ylab="PACF")
tsplot(abs(r), col="red"); acf(abs(r)); pacf(abs(r), ylab="PACF")

### QQ Norm Plot ##
par(mfrow=c(1,1))
qqnorm(r); qqline(r)

### Fitting an ARCH(1) Model ###
# 'fGarch' estimation #
library(fGarch)
summary(msft.arch <- garchFit(~garch(1,20), r, cond.dist = "std"))
# phi0 = 0.0001748
# phi1 = 0.08886
plot(msft.arch, which=3, col=c("red", "blue"), main="")

tsplot(r, ylab="Returns", col="red")
v <- 0
for(t in 2:761)
  v[t] <- 0.0001748 + 0.08886*(r[t-1]^2)

par(mfrow=c(1,1))
tsplot(r, ylab="Returns", col="red")
lines(sqrt(v), col="blue")

# Fitting ARMA(p,q)-ARCH(m) to MSFT
par(mfrow=c(1,1)); tsplot(r)

par(mfrow=c(2,1)); acf(r); pacf(r) # no autocorrelation
par(mfrow=c(2,1)); acf((r-mean(r)^2)); pacf((r-mean(r)^2))

# Hence we will fit an AR(1)-ARCH(1) model to the data
summary(msft.arch <- garchFit(~arma(1,0)+garch(1,0), data=r, cond.dist='std'))
tsplot(r)

acf((r-mean(r))^2)
pacf((r-mean(r))^2)

require(astsa)
getSymbols("SPY", src="yahoo")
price <- (FTSE$FTSE.Close[1:1000])
returns <- diff(log(price))[-1]
ts.plot(returns, col="red")
summary(garchFit(~garch(1,0), returns))
# sigma^2_t = 0.0001670 + 0.4267 * r^2_{t-1}
pred.vals <- 0
for(i in 2:1000)
  pred.vals[i] <- 0.0001670 + 0.4267 * returns[i-1]^2
ts.plot(returns, col="red")
lines((pred.vals-mean(pred.vals))*25, col="blue")
mean(pred.vals)






library(xts)
djiar = diff(log(djia$Close))[-1]
acf2(djiar) # exhibits some autocorrelation (not shown) 
acf2(djiar^2) # oozes autocorrelation (not shown) 
library(fGarch)
summary(djia.g <- garchFit(~garch(1,1), data=djiar,
                           cond.dist='std'))
plot(djia.g, which=3)


getSymbols("^FTSE")
head(FTSE)
FTSE[1000]
p <- ts(FTSE$FTSE.Close[1:1000], start=2007,frequency=253)
r <- ts(diff(log(p))[-1], start=2007,frequency=253)
par(mfrow=c(2,1)); tsplot(p, col="red", ylab="Close price"); tsplot(r, col="red", ylab="Daily returns")
par(mfrow=c(2,1)); acf(r); acf(r^2)
library(fGarch)
summary(FTSE.g <- garchFit(~garch(1,1), data=r, cond.dist='std'))
length(r)

library("astsa")
library('fGarch')
ts.plot(r, volatility(FTSE.g, type="sigma"), -volatility(FTSE.g, type="sigma"), col=c("red", "blue", "blue"))
legend('topleft', col=c("red", "blue"), lty=1, lwd = 2, legend=c("Returns", "Predicted volatility"), bty="n")

acf(r)
pacf(r)
