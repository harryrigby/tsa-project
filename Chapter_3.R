#######################################################
### Chapter 3: Autoregressive Moving Average Models ###
#######################################################
require('quantmod')

#------------------------------------------------------------------------------
# Figure 3.1: MSFT Returns
getSymbols("MSFT", src="yahoo")
ret <- as.vector(diff(log(MSFT$MSFT.Close[1:500]))[-1])
par(mfrow=c(1,1)); ts.plot(ret, col="red", ylab="Returns")

#------------------------------------------------------------------------------
# Fitted ARMA(1,1) model
arima(ret, order=c(1,0,1))

#------------------------------------------------------------------------------
# Causal form of ARMA(1,1) model
round(ARMAtoMA(ar=0.5978, ma=-0.7366, lag.max = 5), 5)

#------------------------------------------------------------------------------
# Figure 3.2: Forecast returns
regr = ar.ols(ret, order=2, demean=FALSE, intercept=TRUE)
fore = predict(regr, n.ahead=15)
par(mfrow=c(1,1)); tsplot(fore$pred, col="red", ylab="Forecast returns"); lines(fore$pred, type="p", col=2)

#------------------------------------------------------------------------------
# Figure 3.3: IMA(1,2) sim
ts.sim <- arima.sim(model=list(order = c(0,1,2), ma = c(1, -0.6)), n = 1000)
tsplot(ts.sim, col="red", ylab="Simulated IMA(1,2) values")

#------------------------------------------------------------------------------
# ADF test
library(tseries)
adf.test(ts.sim, k=0)

#------------------------------------------------------------------------------
# Figure 3.4: Correlation plots of 1st order differences
par(mfrow=c(2,1))
acf(diff(ts.sim))
pacf(diff(ts.sim)) 

#------------------------------------------------------------------------------
# Figure 3.5: Correlation plots of 2nd order differences
par(mfrow=c(2,1))
acf(diff(diff(ts.sim)))
pacf(diff(diff(ts.sim)))

#------------------------------------------------------------------------------
# Table 3.2: Fitted models
arima(x = diff(ts.sim), order=c(0,1,2))
arima(x = diff(diff(ts.sim)), order = c(0,2,2))
arima(x = diff(diff(ts.sim)), order = c(0,2,3))
