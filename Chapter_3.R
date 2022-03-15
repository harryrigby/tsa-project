require('quantmod')
getSymbols('TSLA', src='yahoo') # Source: Yahoo Finance
p <- as.vector(TSLA$TSLA.Close[0:1259]); adf.test(p, k=0)
par(mfrow=c(2,1)); acf((p)); pacf((p))
arima(x = p, order = c(0,1,0))


p <- djia$Close
r <- diff(log(p))
par(mfrow=c(1,1)); ts.plot(r, ylab="Close Price", col="red")

?arima.sim()
ts.sim <- arima.sim(model=list(order = c(0,1,2), ma = c(1, -0.6)), n = 1000)

par(mfrow=c(1,1))
tsplot(ts.sim, col="red", ylab="Simulated IMA(1,2) values")
par(mfrow=c(2,1))
acf(ts.sim) # Tails off
pacf(ts.sim) # cuts after lag 1 => AR(1) ?
arima(x = ts.sim, order=c(1,0,0))
# ar1 = 0.9994, sigma^2 = 2.263, aic = 3671.03

ts.plot(diff(ts.sim))
acf(diff(ts.sim)) # cuts after lag 2
pacf(diff(ts.sim)) # tails off => IMA(1,2) ?
arima(x = diff(ts.sim), order=c(0,1,2))
# ma1 = -0.6129, ma2 = -0.3850, sigma^2 = 2.114, aic = 3594.13

ts.plot(diff(diff(ts.sim)))
acf(diff(diff(ts.sim))) # cuts after lag 3/4
pacf(diff(diff(ts.sim))) # tails off => IMA(2,3) ?
arima(x = diff(diff(ts.sim)), order = c(0,2,3))
# ma1 = -2.7156, ma2 = 2.4358, ma3 = -0.7199, sigma^2 = 2.69, aic = 3844.08
arima(x = diff(diff(ts.sim)), order = c(0,2,4))

library(tseries)
adf.test(ts.sim, k=0)


?arima
arima

par(mfrow=c(1,1))
set.seed(2); ts.plot(arma <- arima.sim(model=list(order = c(1,0,1), ar=0.75, ma=0.5), n = 1000), ylab="ARMA(1,1)", col="red", ylim=c(-7,7))
abline(0, 0, col=gray(0.9))
acf(arma)
mean(arma)

ARMAtoMA()

getSymbols("MSFT", src="yahoo")
ret <- as.vector(diff(log(MSFT$MSFT.Close[1:500]))[-1]); par(mfrow=c(1,1)); ts.plot(ret, col="red", ylab="Returns")
par(mfrow=c(2,1)); acf(ret); pacf(ret)
arima(ret, order=c(1,0,1))
mean(ret)
arima(ret-mean(ret), order=c(1,0,1))

MSFT$MSFT.Close[500]

regr = ar.ols(ret, order=2, demean=FALSE, intercept=TRUE)
fore = predict(regr, n.ahead=24)
ts.plot(ret, fore$pred, col=1:2)
U = fore$pred+fore$se;  L = fore$pred-fore$se
xx = c(time(U), rev(time(U)));  yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(fore$pred, type="p", col=2)

ret <- as.vector(diff(log(MSFT$MSFT.Close[1:300]))[-1]); par(mfrow=c(1,1)); ts.plot(ret, col="red", ylab="Returns")
par(mfrow=c(2,1)); acf(ret); pacf(ret)
arima(ret, order=c(1,0,1))

round(ARMAtoMA(ar=0.5978, ma=-0.7366, lag.max = 5), 5)
      
      
      