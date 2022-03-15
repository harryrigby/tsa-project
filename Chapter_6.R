#######################################
### Chapter 6: Hidden Markov Models ###
#######################################

### Packages ###
install.packages("depmixS4")
library(depmixS4)
help(depmixS4)
set.seed(2)

### Model Fit to Data ###
set.seed(2)
y = ts(diff(log(djia$Close[178:1178]))[-1], start=2007, freq=253) # djia data from 'astsa' package
mod3 <- depmix(y~1, nstates=3, data=data.frame(y))
summary(fm3 <- fit(mod3))

### State Prediction Using HMM Smooher ###
tsplot(y, main="", ylab='DJIA Daily Returns', col=gray(.7),
       ylim=c(-.11,.11))
culer = 4-posterior(fm3)[,1]; culer[culer==3]=4 # switch labels 1 and 3 
text(y, col=culer, labels=4-posterior(fm3)[,1])

### Histogram Plot ###
para.mle = as.vector(getpars(fm3)[-(1:3)])
permu = matrix(c(0,0,1,0,1,0,1,0,0), 3,3) 
(mtrans.mle = permu%*%round(t(matrix(para.mle[1:9],3,3)),3)%*%permu) 
(norms.mle = round(matrix(para.mle[10:15],2,3),3)%*%permu)

# 2 state model
mod2 <- depmix(y~1, nstates=2, data=data.frame(y))x
summary(fm2 <- fit(mod2))
logLik(mod2) # -919.0631
-2*L2+ (2)^2 + 2*(2) - 1                            # AIC = 1845.126
-2*logLik(mod2)+ ((2)^2 + 2*(2) - 1)*log(length(y)) # BIC = 1886.48
tsplot(y, main="", ylab='DJIA Daily Returns', col=gray(.7),
       ylim=c(-.11,.11))
culer = 3-posterior(fm2)[,1];
text(y, col=culer, labels=3-posterior(fm2)[,1])
legend('topleft', col=c("black", "red"), lty=0, pch=20, legend=c("State 1", "State 2"), bty="n")

para.mle = as.vector(getpars(fm2)[-(1:2)])
permu = matrix(c(0,1,1,0), 2,2) 
(mtrans.mle = permu%*%round(t(matrix(para.mle[1:4],2,2)),2)%*%permu) 

hist(y, breaks=20, xlim=c(-0.11, 0.11), xlab="Returns", main="", ylim=c(0,350))


hist# 3 state
mod3 <- depmix(y~1, nstates=3, data=data.frame(y))
summary(fm3 <- fit(mod3))
logLik(mod3) # -919.0631
-2*logLik(mod3)+ (3)^2 + 2*(3) - 1                  # AIC = 1852.126
-2*logLik(mod3)+ ((3)^2 + 2*(3) - 1)*log(length(y)) # BIC = 1934.835

# 4 state
mod4 <- depmix(y~1, nstates=4, data=data.frame(y))
summary(fm4 <- fit(mod4))
logLik(mod4)
-2*logLik(mod4)+ (4)^2 + 2*(4) - 1             # AIC = 1861.126
-2*logLik(mod4)+ ((4)^2 + 2*(4) - 1)*log(1000) # BIC = 1997.005

# 5 state
mod5 <- depmix(y~1, nstates=5, data=data.frame(y))
summary(fm5 <- fit(mod5))
logLik(mod5)
-2*logLik(mod5)+ (5)^2 + 2*(5) - 1             # AIC = 1872.126
-2*logLik(mod5)+ ((5)^2 + 2*(5) - 1)*log(1000) # BIC = 2072.99



para.mle = as.vector(getpars(fm2)[-(1:2)])
permu = matrix(c(0,1,1,0), 2,2) 
(mtrans.mle = permu%*%round(t(matrix(para.mle[1:4],2,2)),3)%*%permu) 
(norms.mle = (matrix(para.mle[7:10],2,2)))
hist(y, 28, prob=TRUE, main='', xlab='DJIA Daily Returns', ylab='Density', xlim=c(-0.06,0.06))
culer=c(1,2,4); pi.hat = colSums(posterior(fm2)[-1,2:4])/length(y); for (i in 1:2) { 
  mu=norms.mle[1,i]; sig = norms.mle[2,i]
  x = seq(-.15,.12, by=.001)
  lines(x, pi.hat[3-i]*dnorm(x, mean=mu, sd=sig), col=culer[i])
}





x <- seq(min(y), max(y), length=100)
s1 <- dnorm(x, mean=0, sd=0.01)
s2 <- dnorm(x, mean=-0.002, sd=0.03)
hist(y, breaks=25, xlim=c(-0.1, 0.1), xlab="Returns", main="", ylim=c(0,40), freq = FALSE)
lines(x, s1, col="black")
lines(x, s2, col="red")
legend('topleft', col=c("black", "red"), lty=1, lwd = 2, 
       legend=c("State 1 (low volatility)", "State 2 (high volatility)"), bty="n")
