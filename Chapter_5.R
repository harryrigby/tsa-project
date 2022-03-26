#######################################
### Chapter 5: Hidden Markov Models ###
#######################################

# ------------------------------------------------------------------------------
# Figure 5.3: HMM sim
set.seed(2)
t <- rbinom(100, 1, 0.5)
returns_function <- function(i){return(if(t[i]<1) print(rnorm(1, 0, 0.5)) else print(rnorm(1, 0, 3)))}
returns <- as.vector(sapply(1:100, returns_function))

tsplot(returns, ylim=c(-max(abs(min(returns)), abs(max(returns)))-0.9, max(abs(min(returns)), abs(max(returns)))+0.9), 
       col=grey(0.7), ylab="returns"); points(returns, col=ifelse(t>0, "red","green"), type="p", pch=20)
legend('topleft', col=c("green", "red"), lwd=1, 
       pch=20, legend=c("State 1 (low variance)", "State 2 (high variance)"), bty="n")

# ------------------------------------------------------------------------------
# Figure 5.4: HMM sim 2
install.packages("seqHMM")
require("seqHMM")

emission_probs <- matrix(c(1, 0, 0, 1), 2, 2, byrow=TRUE)
transition_probs <- matrix(c(0.99, 0.01, 0.05, 0.95), 2, 2, byrow=TRUE)
initial_probs <- c(1, 0) # HMM parameters

set.seed(4)
sim <- simulate_hmm(
  n_sequences = 1, initial_probs = c(1,0), 
  transition_probs = transition_probs, 
  emission_probs = emission_probs, 
  sequence_length = 250)
states <- as.numeric(sim$states); states # simulated states

returns_function <- function(i){return(if(states[i]<2) print(rnorm(1, 0, 0.5)) else print(rnorm(1, 0, 3)))}
returns <- as.vector(sapply(1:length(states), returns_function))

tsplot(returns, ylim=c(-max(abs(min(returns)), abs(max(returns)))-0.9, max(abs(min(returns)), abs(max(returns)))+0.9), 
       col=grey(0.7), ylab="returns"); points(returns, col=ifelse(states>1, "red","green"), type="p", pch=20)
legend('topright', col=c("green", "red"), lwd=1, 
       pch=20, legend=c("State 1 (low variance)", "State 2 (high variance)"), bty="n")

# ------------------------------------------------------------------------------
# Figure 5.5: Vertibi path
install.packages("depmixS4")
library("depmixS4")
y <- ts(returns, start=1, frequency = 1)
fitted.hmm <- depmix(y~1, nstates=2, data=data.frame(y))
summary(fm3 <- fit(fitted.hmm))

tsplot(y, ylab="returns", col=gray(.7))
culer <- posterior(fm3, type="viterbi")[,1]; factor(culer, levels=c(2,1), labels=c("green", "red"))
text(y, col=c(factor(culer, levels=c(2,1), labels=c("green", "red"))), labels=3-posterior(fm3, type="viterbi")[,1])

# ------------------------------------------------------------------------------
# Figure 5.6: DJIA daily returns
library("astsa")
y = ts(diff(log(djia$Close[178:1178]))[-1], start=2007, freq=253)
tsplot(y, ylab="DJIA Daily Returns", col="red")

# ------------------------------------------------------------------------------
# Table 5.1: 2,3,4 state models
# 2 state model
mod2 <- depmix(y~1, nstates=2, data=data.frame(y))
summary(fm2 <- fit(mod2))

# 3 state model
mod3 <- depmix(y~1, nstates=2, data=data.frame(y))
summary(fm3 <- fit(mod3))

# 4 state model
mod4 <- depmix(y~1, nstates=4, data=data.frame(y))
summary(fm4 <- fit(mod4))

# ------------------------------------------------------------------------------
# Figure 5.7: 3-state plot
tsplot(y, ylab='DJIA Daily Returns', col=gray(.7), ylim=c(-.11,.11))
culer = 4-posterior(fm3)[,1];
text(y, col=culer, labels=posterior(fm3)[,1])

# ------------------------------------------------------------------------------
# Figure 5.8: 4-state plot
tsplot(y, ylab='DJIA Daily Returns', col=gray(.7), ylim=c(-.11,.11))
culer = 5-posterior(fm4)[,1];
text(y, col=culer, labels=posterior(fm4)[,1])





