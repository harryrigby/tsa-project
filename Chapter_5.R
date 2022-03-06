#####################################
### Chapter 5: State Space Models ###
#####################################

# Figure 5.X: HMM Fitting
install.packages("depmixS4") # package used for fitting hmm
library(depmixS4)
library(astsa)
set.seed(2)
y = ts(diff(log(djia$Close[178:1178]))[-1], start=2007, freq=253) # djia data from 'astsa' package
mod3 <- depmix(y~1, nstates=3, data=data.frame(y)) # fitting 3-state model
summary(fm3 <- fit(mod3))

tsplot(y, ylab='DJIA Daily Returns', col="red", ylim=c(-.11,.11)) 

tsplot(y, ylab='DJIA Daily Returns', col=gray(.7), ylim=c(-.11,.11)) 
culer = 4-posterior(fm3)[,1]; culer[culer==3]=4 # switch labels 1 and 3 
text(y, col=culer, labels=4-posterior(fm3)[,1]) # add state labels

para.mle = as.vector(getpars(fm3)[-(1:3)])
permu = matrix(c(0,0,1,0,1,0,1,0,0), 3,3) 
(mtrans.mle = permu%*%round(t(matrix(para.mle[1:9],3,3)),3)%*%permu) 
(norms.mle = round(matrix(para.mle[10:15],2,3),3)%*%permu)





# HMM Sim #
# 2-state model: state 1 = normal volatility; state 2 = high volatility
# (y_t | s=1) ~ N(0, 1)
# (y_t | s=2) ~ N(0, 5)                            
# Transition probs:                             
# p(s1->s1) = 0.9; p(s1->s2) = 0.1     
# p(s2->s1) = 0.1; p(s2->s2) = 0.9           
# Transition matrix:
#       s1  s2
# s1 |0.9  0.1|
# s2 |0.1  0.9|

# state 1 returns
set.seed(2)
s1 <- rnorm(100)

# state 2 returns
s2 <- rnorm(length(s1), 0, sqrt(5))

# state transitions
t <- rbinom(length(s1), 1, 0.5); t

# initial state at t=0 = 1
set.seed(2)
t <- rbinom(100, 1, 0.5)
returns_function <- function(i){return(if(t[i]<1) print(rnorm(1, 0, 0.5)) else print(rnorm(1, 0, 3)))}
returns <- as.vector(sapply(1:100, returns_function))
tsplot(returns, ylim=c(-max(abs(min(returns)), abs(max(returns)))-0.9, max(abs(min(returns)), abs(max(returns)))+0.9), 
       col=grey(0.7), ylab="returns"); points(returns, col=ifelse(t>0, "red","green"), type="p", pch=20)
legend('topleft', col=c("green", "red"), lwd=1, 
        pch=20, legend=c("State 1 (low variance)", "State 2 (high variance)"), bty="n")
abline(0,0)
?legend
?plot
warnings()

# fitting model to simulated data
summary(fit(fit.ret <- depmix(response=returns~1, nstates=3, data=data.frame(returns))))
summary(fm2 <- fit(fit.ret))
# aic = -2log(L) + 2p;  p = (# states)^2 + (# parameters of state process)*(# states) - 1
2*183.2081 + 2*(2^2 + 4*2 - 1)

2*180.4452 + 2*(3^2 + 6*3 - 1)

install.packages("seqHMM")
require("seqHMM")
# Parameters for the HMM
emission_probs <- matrix(c(1, 0, 0, 1), 2, 2)
transition_probs <- matrix(c(0.95, 0.05, 0.1, 0.9), 2, 2)
initial_probs <- c(1, 0)

# Simulating sequences
sim <- simulate_hmm(
  n_sequences = 1, initial_probs = c(0.5,0.5), 
  transition_probs = transition_probs, 
  emission_probs = emission_probs, 
  sequence_length = 50)

states <- as.numeric(sim$states) # simulated states
ssplot(sim)


