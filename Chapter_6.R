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
text(y, col=culer, labels=posterior(fm3)[,1])
ts.plot(posterior(fm4, type="viterbi")[,1]) # state 3 from 421 to 566 

T <- matrix(c(0.912, 0, 0.088,0, 0.992, 0.008, 0.042, 0.002, 0.956),3,3,byrow=TRUE)
T

### Histogram Plot ###
para.mle = as.vector(getpars(fm3)[-(1:3)])
permu = matrix(c(0,0,1,0,1,0,1,0,0), 3,3) 
(mtrans.mle = permu%*%round(t(matrix(para.mle[1:9],3,3)),3)%*%permu) 
(norms.mle = round(matrix(para.mle[10:15],2,3),3)%*%permu)

# 2 state model
fit(mod2 <- depmix(y~1, nstates=2, data=data.frame(y)))
summary(fm2 <- fit(mod2))
logLik(mod2) # -223.21
-2*(-223.21)+ (2)^2 + 2*(2) - 1                     # AIC = 453.42
-2*(-223.21)+ ((2)^2 + 2*(2) - 1)*log(length(y))    # BIC = 485.0702

tsplot(y, main="", ylab='DJIA Daily Returns', col=gray(.7),
       ylim=c(-.11,.11))
culer = 3-posterior(fm2)[,1];
text(y, col=culer, labels=3-posterior(fm2)[,1])
legend('topleft', col=c("black", "red"), lty=0, pch=20, legend=c("State 1", "State 2"), bty="n")

para.mle = as.vector(getpars(fm2)[-(1:2)])
permu = matrix(c(0,1,1,0), 2,2) 
(mtrans.mle = permu%*%round(t(matrix(para.mle[1:4],2,2)),2)%*%permu) 

hist(y, breaks=20, xlim=c(-0.11, 0.11), xlab="Returns", main="", ylim=c(0,350))


# 3 state
fit(mod3 <- depmix(y~1, nstates=3, data=data.frame(y)))
summary(fm3 <- fit(mod3))
logLik(mod3) # -216.326
-2*-216.326+ (3)^2 + 2*(3) - 1                  # AIC = 446.652
-2*-216.326+ ((3)^2 + 2*(3) - 1)*log(length(y)) # BIC = 509.9525

# 4 state
fit(mod4 <- depmix(y~1, nstates=4, data=data.frame(y)))
summary(fm4 <- fit(mod4))
logLik(mod4)
-2*(-212.539) + (4)^2 + 2*(4) - 1                 # AIC = 448.078
-2*(-212.539)+ ((4)^2 + 2*(4) - 1)*log(length(y)) # BIC = 552.0716

# 5 state
fit(mod5 <- depmix(y~1, nstates=5, data=data.frame(y)))
summary(fm5 <- fit(mod5))
logLik(mod5)
-2*(-209.2614 )+ (5)^2 + 2*(5) - 1                  # AIC = 452.5228
-2*(-209.2614 )+ ((5)^2 + 2*(5) - 1)*log(length(y)) # BIC = 606.2525



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


tsplot(y, main="", ylab='DJIA Daily Returns', col=gray(.7),
       ylim=c(-.11,.11))
culer = 5-posterior(fm4)[,1];
text(y, col=culer, labels=posterior(fm4)[,1])

VP <- posterior(fm4, type="viterbi")[,1]
fun <- function(VP){return(if(abs(VP[i+1]-VP[i])<1) print(0) else print(1))}
sapply(2:1000, FUN = fun)


for(i in 1:1000)
  x[i] <- if(abs(VP[i+1]-VP[i])>0) print(1) else print(0)
table(x)[2] # total no. of state changes (i.e. no. of 1s in x) = 371

x <- c("")


