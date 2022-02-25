##############################################
### TIME SERIES ANALYSIS: CHAPTER 1 R CODE ###
##############################################

# Figure 1.1: AMZN close stock price plot
install.packages('astsa')
install.packages('quantmod') #'quantmod' package used to import data into R
require('quantmod')
getSymbols('AMZN', src='yahoo') # Source: Yahoo Finance
par(mfrow=c(1,1)) 
AMZN <- ts(AMZN$AMZN.Close, start=2007, frequency = 253) # Setting time scale
tsplot(AMZN, ylab="Close Price (USD$)", col="red")