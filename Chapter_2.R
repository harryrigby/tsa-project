##############################################
### TIME SERIES ANALYSIS: CHAPTER 2 R CODE ###
##############################################

# Figure 2.1: Gaussian white noise
set.seed(2)
w <- rnorm(300) # 300 random standard normal variates
par(mfrow=c(1,1))
tsplot(w, col="red", ylab="White noise", ylim=c(-3,3))


# Figure 2.2: 3-point moving average
v <- filter(w, sides=1, filter=rep(1/3, 3))[-1] # 3-pt moving average
v[1] <- 0 # Set v1=0
par(mfrow=c(1,1))
tsplot(v, col="red", ylab="Moving average", ylim=c(-3,3))


# Figure 2.3: Actual vs simulated ACF plots
y_w <- c(1,0,0,0,0,0,0,0,0,0,0) # WN expected ACF
y_v <- c(1,2/3,1/3,0,0,0,0,0,0,0,0) # MA expected ACF
x <- c(0:10) # no. of lags
par(mfrow=c(2,2))
plot(x,y_w, type="h", xlab="Lag", ylab="Expected WN ACF", ylim=c(-0.12,1.02)); abline(0,0)
acf(w, lag.max=10, ylab="Simulated WN ACF")
plot(x,y_v, type="h", xlab="Lag", ylab="Expected MA ACF", ylim=c(-0.12,1.02)); abline(0,0)
acf(v, lag.max=10, ylab="Simulated MA ACF")


# Figure 2.4: AMZN stock price 30/12/2011 to 3/12/2013
install.packages('quantmod') #'quantmod' package used to import data into R
require('quantmod')
getSymbols('AMZN', src='yahoo') # Source: Yahoo Finance
Amazon <- as.vector(AMZN$AMZN.Close[1260:1743]) # 30/12/2011 to 3/12/2013
par(mfrow=c(1,1))
tsplot(Amazon, ylab="Close Price", col="red")

# Figure 2.5: ACF of 1st order differences
par(mfrow=c(1,1))
acf(diff(Amazon))


# Figure 2.6: AR(2) model with ACF and PACF
a <- 0.5; b=1; c=-0.5 # model parameters
w=rnorm(300) # 300 gaussian white noise terms
x <- w
for(t in 3:300)
  x[t] <- a + b*x[t-1] + c*x[t-2] + w[t]
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(x, type="l", col="red", xlab="Time")
acf(x)
pacf(x, ylab="PACF")


# Figure 2.7: AMZN price with regression line
# Model 1 (p = b_0 + b_1*t + e)
k1 <- 2 # number of model parameters
Time <- 1:length(Amazon) # Number of data points
fit1 <- lm(Amazon ~ Time)
summary(fit1)
par(mfrow=c(1,1))
tsplot(x=Time, y=Amazon, ylab="AMZN Close Price", col="red") # Price plot
abline(fit1, col="blue") # Regression line
legend('topleft', col=c("red", "blue"), lty=1, lwd = 2, legend=c("Close price", "Linear Model"), bty="n")


# Figure 2.8: Models 1-5
# Model 1 (p = b_0 + b_1*t + e)
k1 <- 2 # number of model parameters
Time <- 1:length(Amazon) # Number of data points
fit1 <- lm(Amazon ~ Time)
summary(fit1)

sse1 <- sum((fitted(fit1)-Amazon)^2); mse1 <- sse1/fit1$df # SSE and MSE
sse1; mse1

AIC1 <- log(sse1/n)+(n+2*k1)/n; BIC1 <- log(sse1/n)+k1*log(n)/n # Information criterions
AIC1; BIC1

# Model 2 (p = b_0 + b_1*t + b_2*t^2 + e)
k2 <- 3
fit2 <- lm(Amazon ~ Time+I(Time^2))
summary(fit2)

sse2 <- sum((fitted(fit2)-Amazon)^2); mse2 <- sse2/fit2$df
sse2; mse2 

AIC2 <- log(sse2/n)+(n+2*k2)/n; BIC2 <- log(sse2/n)+k2*log(n)/n
AIC2; BIC2


# Model 3 (p = b_0 + b_1*t + b_2*t^2 + b_3*t^3 + e)
k3 <- 4 
fit3 <- lm(Amazon ~ Time+I(Time^2)+I(Time^3))
summary(fit3)

sse3 <- sum((fitted(fit3)-Amazon)^2); mse3 <- sse3/fit3$df
sse3; mse3

AIC3 <- log(sse3/n)+(n+2*k3)/n; BIC3 <- log(sse3/n)+k3*log(n)/n
AIC3; BIC3

# Model 4 (p = b_0 + b_1*t + b_2*t^2 + b_3*t^3 + b_4*t^4 + e) 
k4 <- 5
fit4 <- lm(Amazon ~ Time+I(Time^2)+I(Time^3)+I(Time^4))
summary(fit4)

sse4 <- sum((fitted(fit4)-Amazon)^2); mse4 <- sse4/fit4$df
sse4; mse4

AIC4 <- log(sse4/n)+(n+2*k4)/n; BIC4 <- log(sse4/n)+k4*log(n)/n
AIC4; BIC4

# Model 5 (p = b_0 + b_1*t + b_2*t^2 + b_3*t^3 + b_4*t^4 + b_5*t^5 + e)
k5 <- 6
fit5 <- lm(Amazon ~ Time+I(Time^2)+I(Time^3)+I(Time^4)+I(Time^5))
summary(fit5)

sse5 <- sum((fitted(fit5)-Amazon)^2); mse5 <- sse5/fit5$df
sse5; mse5

AIC5 <- log(sse5/n)+(n+2*k5)/n;BIC5 <- log(sse5/n)+k5*log(n)/n
AIC5; BIC5


# Figure 2.9: Model 4 summary
summary(fit4)


# Figure 2.10: AMZN plot with model 4 
func <- function(t){return(
  fit4$coefficients[1]+fit4$coefficients[2]*t+fit4$coefficients[3]*t^2
  +fit4$coefficients[4]*t^3+fit4$coefficients[5]*t^4)
}
fit4vals <- sapply(Time, func)
par(mfrow=c(1,1))
tsplot(x=Time, y=Amazon, ylab="AMZN Close Price", col="red")
lines(fit4vals, col="blue")
legend('topleft', col=c("red", "blue"), lty=1, lwd = 2, legend=c("Close price", "Model 4"), bty="n")


install.packages("zoo")
library("zoo")
length(AMZN)
tsplot(AMZN[3717:3813], col="red", ylab="AMZN")
legend('topright', col=c("red", "blue", "green"), lty=1, lwd = 2, legend=c("Close price", "10 day MA", "50 day MA"), bty="n")
lines(rollmean(AMZN[3707:3813], 10), col="blue", type="l")
lines(rollmean(AMZN[3667:3813], 50), col="green", type="l")

# 16/11/2021 - 24/2/2022
AMZN[3717:3813]
# day 20: 3312.75
# day 33: 3676.57


