##################################
# Homework 1 - Question 1
##################################

rm(list=ls())

# Packages
library(xts)
library("TTR")
library(tidyverse)
library(ggplot2)
library(reshape2)

## Set working directory to current script's location.
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
rm(fileloc)

## Load data
world_markets_data <- readRDS('data/WorldMarkts99_20.RDS')

## Extract BSESN data
bsesn <-world_markets_data$BSESN

## Get subset of data 
bsesn_df <- bsesn['2015-04/2017-03']; m=length(bsesn_df$BSESN.Open); 
bsesn_ophlc <- bsesn_df[,c("BSESN.Open","BSESN.High","BSESN.Low","BSESN.Close")]

# Fill in missing values with previous value
bsesn_ophlc$BSESN.Open <- zoo::na.locf(bsesn_ophlc$BSESN.Open, na.rm = FALSE)
bsesn_ophlc$BSESN.High <- zoo::na.locf(bsesn_ophlc$BSESN.High, na.rm = FALSE)
bsesn_ophlc$BSESN.Low <- zoo::na.locf(bsesn_ophlc$BSESN.Low, na.rm = FALSE)
bsesn_ophlc$BSESN.Close <- zoo::na.locf(bsesn_ophlc$BSESN.Close, na.rm = FALSE)

## Calculate volatility (regular estimations)
######################
vClose <- TTR::volatility(bsesn_ophlc, n=252,calc="close",N=252)
vParkinson <- TTR::volatility(bsesn_ophlc, n= 252,calc="parkinson",N=252)
vGK <- TTR::volatility(bsesn_ophlc, n= 252,calc="garman",N=252)


## Calculate EMA using TTR
######################
returns <-diff(bsesn_ophlc$BSESN.Close)
log_returns <- diff(log(bsesn_ophlc$BSESN.Close))
log_returns_squared <- log_returns^2
vEMA <- TTR::EMA(log_returns_squared, ratio=0.06, N=252)
vEMA <- sqrt(vEMA) * sqrt(252)  # Convert to annualized volatility

vEMA[m] # 0.09

# Create a data frame of results
volatility_measures <- data.frame(
  Close = vClose[m],
  Parkinson = vParkinson[m],
  Garman_Klass = vGK[m],
  EMA = vEMA[m]
)
print(volatility_measures)



# Plot
#######

# Combine the xts objects into a single xts object
combined_xts <- merge(vClose, vParkinson, vGK, vEMA)
combined_xts <- combined_xts['2016-04/2017-03']

# Convert the xts object to a data frame for ggplot
df_combined <- data.frame(Date = index(combined_xts), coredata(combined_xts))

# Melt the data frame for ggplot2
df_melted <- melt(df_combined, id = "Date")

# Plot using ggplot2
ggplot(df_melted, aes(x = Date, y = value, color = variable)) +
  geom_line() +
  labs(title = "Volatity of BSESN",
       x = "Date", y = "Value", color = "Variables") +
  theme_minimal()




