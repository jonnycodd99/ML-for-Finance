##################################
# Homework 1 - Question 1
##################################

rm(list=ls())

# Packages
library(xts)
library("TTR")

## Set working directory to current script's location.
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

## Load data
world_markets_data <- readRDS('data/WorldMarkts99_20.RDS')

## Extract BSESN data
bsesn <-world_markets_data$BSESN

## Get subset of data
bsesn_df <- bsesn['2016-04/2017-03']; m=length(bsesn_df$BSESN.Open); 
bsesn_ophlc <- bsesn_df[,c("BSESN.Open","BSESN.High","BSESN.Low","BSESN.Close")]

## Calculate volatility (regular estimations)
######################
vClose <- TTR::volatility(bsesn_ophlc, n=m,calc="close",N=252)
vParkinson <- TTR::volatility(bsesn_ophlc, n= m,calc="parkinson",N=252)
vGK <- TTR::volatility(bsesn_ophlc, n= m,calc="garman",N=252)

## Calculate EMA using TTR
######################
log_returns <- diff(log(bsesn_ophlc$BSESN.Close))
log_returns_squared <- log_returns^2
vEMA <- TTR::EMA(log_returns_squared, ratio=0.06, N=252)
vEMA <- sqrt(vEMA) * sqrt(252)  # Convert to annualized volatility

vEMA[m] # 0.903

# Create a data frame of results
volatility_measures <- data.frame(
  Close = vClose[m],
  Parkinson = vParkinson[m],
  Garman_Klass = vGK[m],
  EMA = vEMA[m]
)
print(volatility_measures)


## Manual Calculation as sense check
######################
# Calculate log returns
log_returns <- log_returns[-1]

# Set the decay factor 
lambda <- 0.94

# Initialize the array for EWMA volatility with the first element as the initial variance
ewma_vol <- numeric(length(log_returns))
ewma_vol[1] <- var(log_returns)  # Initial variance estimate 

# Compute EWMA for volatility
for (i in 2:length(log_returns)) {
  ewma_vol[i] <- lambda * ewma_vol[i - 1] + (1 - lambda) * (log_returns[i-1]^2)
}

# Convert variance to volatility and annualize it
ewma_vol <- sqrt(ewma_vol) * sqrt(252)  # Convert to annualized volatility
ewma_vol[m-1] # 0.093
  




