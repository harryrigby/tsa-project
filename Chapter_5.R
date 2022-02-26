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

tsplot(y, ylab='DJIA Daily Returns', col=gray(.7), ylim=c(-.11,.11)) 
culer = 4-posterior(fm3)[,1]; culer[culer==3]=4 # switch labels 1 and 3 
text(y, col=culer, labels=4-posterior(fm3)[,1]) # add state labels

para.mle = as.vector(getpars(fm3)[-(1:3)])
permu = matrix(c(0,0,1,0,1,0,1,0,0), 3,3) 
(mtrans.mle = permu%*%round(t(matrix(para.mle[1:9],3,3)),3)%*%permu) 
(norms.mle = round(matrix(para.mle[10:15],2,3),3)%*%permu)