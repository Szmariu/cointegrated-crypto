###################################################
# Preparation 
###################################################

# Libraries
library(tidyverse)
library(ggplot2) # nice plots
library(ggthemes) 
library(ggtech) # For the airbnb theme

library(xts)
library(lmtest)
library(tseries)
library(urca)
library(fUnitRoots)
library(quantmod)
library(fBasics)
library(forecast)



# For better plotting
theme_set(theme_airbnb_fancy())
orange = "#FFB400"
purple = "#7B0051"

options(scipen  =  10)

# Fix for the month shorthands
Sys.setlocale("LC_ALL","English")

source("functions.R")


###################################################
# Data  
###################################################
btc <- getCryptoHistoricalPrice("bitcoin")
eth <- getCryptoHistoricalPrice("ethereum")

### Basic ETA ### 
# Shape
btc %>% tail
eth %>% tail

# Summary
summary(btc)
summary(eth)

# Basic plots
btc %>%
	ggplot(aes(Date, Close)) +
	geom_area(fill = orange, color = orange, alpha = 0.5) +
	labs(title = "Bitcoin historical prices") +
	theme(
		text = element_text(family = "sans"), # Remove the font warnings
		plot.title = element_text(family = "sans")
	)

eth %>%
	ggplot(aes(Date, Close)) +
	geom_area(fill = orange, color = orange, alpha = 0.5) +
	labs(title = "Etherium historical prices") +
	theme(
		text = element_text(family = "sans"), # Remove the font warnings
		plot.title = element_text(family = "sans")
	)

btc$source <- 'Bitcoin'
eth$source <- 'Etherium'

btc$ClosePercent <- btc$Close / max(btc$Close)
eth$ClosePercent <- eth$Close / max(eth$Close)


bind_rows(btc, eth) %>%
	ggplot(aes(Date, 
			   ClosePercent,
			   color = source)) +
	geom_line() +
	scale_color_manual(values = c(orange, purple)) +
	labs(title = "Close price as % of max price for each coin") +
	theme(
		text = element_text(family = "sans"), # Remove the font warnings
		plot.title = element_text(family = "sans")
	)

bind_rows(btc, eth) %>%
	ggplot(aes(Date, ClosePercent)) +
	geom_line(color = orange) +
	labs(title = "Close price as % of max price for each coin",
		 subtitle = 'Colored only the difference') +
	theme(
		text = element_text(family = "sans"), # Remove the font warnings
		plot.title = element_text(family = "sans")
	)

data <- data.frame(
	date = btc$Date,
	btc  = btc$Close,
	eth  = eth$Close
)

remove(btc)
remove(eth)  

data$Dbtc <- diff.xts(data$btc)
data$Deth <- diff.xts(data$eth)

# Drop the first row with NAs
data <- data %>% drop_na()

###################################################
# ARIMA
###################################################

### Bitcoin ###
# Stationarity
testdf(variable = data$btc,
	   max.augmentations = 30)

testdf(variable = data$Dbtc,
	   max.augmentations = 30)

# ACF and PACF
par(mfrow = c(2, 1)) 
acf(data$Dbtc,
	lag.max = 36, 
	ylim = c(-0.1, 0.1),    
	lwd = 5,               
	col = orange) 

pacf(data$Dbtc, 
	 lag.max = 36, 
	 lwd = 5,
	 col = orange)
par(mfrow = c(1, 1)) 

# Gotta use the 6 core processor on something
# Seems that (5,1,0) is the best
auto.arima(data$Dbtc,
		   d = 1,
		   max.p = 240,
		   max.q = 240,
		   stepwise = FALSE,
		   parallel = TRUE,
		   num.cores = 12)

# But let's try something bigger
BTC.arima2010 <- Arima(data$btc,  
				  order = c(20, 1, 0)  
)

# 1, 9, 18 and 20 are significant
coeftest(BTC.arima2010)

# Let's estimate it with only the significant ones
BTC.arima2010.small <- Arima(data$btc,  
					  		order = c(20, 1, 0),
							fixed = c(
								NA, #1
								rep(0, 7),
								NA, #9
								rep(0, 8),
								NA, # 18
								0,
								NA # 20
							)
)

# Looking better
coeftest(BTC.arima2010.small)

# Plot resudials
plot(resid(BTC.arima2010.small))

# No significance, good
par(mfrow = c(2, 1)) 
acf(resid(BTC.arima2010.small), 
	lag.max = 36,
	ylim = c(-0.1, 0.1), 
	lwd = 5, col = orange,
	na.action = na.pass)
pacf(resid(BTC.arima2010.small), 
	 lag.max = 36, 
	 lwd = 5, col = orange,
	 na.action = na.pass)
par(mfrow = c(1, 1))

# Residuals are definetly not correlated
Box.test(resid(BTC.arima510), type = "Ljung-Box", lag = 10)





### Etherium ###
# Stationarity
testdf(variable = data$eth,
	   max.augmentations = 30)

testdf(variable = data$Deth,
	   max.augmentations = 30)

# ACF and PACF
par(mfrow = c(2, 1)) 
acf(data$Deth,
	lag.max = 36, 
	ylim = c(-0.1, 0.1),    
	lwd = 5,               
	col = orange) 

pacf(data$Deth, 
	 lag.max = 36, 
	 lwd = 5,
	 col = orange)
par(mfrow = c(1, 1)) 

# Gotta use the 6 core processor on something
# Also (5,1,0) 
auto.arima(data$Deth,
		   d = 1,
		   max.p = 240,
		   max.q = 240,
		   stepwise = FALSE,
		   parallel = TRUE,
		   num.cores = 12)

# But let's try something bigger
ETC.arima2010 <- Arima(data$eth,  
					   order = c(20, 1, 0)  
)

# Other are significant
coeftest(ETC.arima2010)

# Let's estimate it with only the significant ones
ETC.arima2010.small <- Arima(data$btc,  
							 order = c(20, 1, 0),
							 fixed = c(
							 	NA, #1
							 	rep(0, 2),
							 	NA, #4
							 	NA, #5
							 	rep(0, 4),
							 	NA, #10
							 	rep(0, 9),
							 	NA # 20
							 )
)

# Some stopped being significant
coeftest(ETC.arima2010.small)

# New model
ETC.arima2010.small <- Arima(data$btc,  
							 order = c(20, 1, 0),
							 fixed = c(
							 	NA, #1
							 	rep(0, 18),
							 	NA # 20
							 )
)

# Both significant
coeftest(ETC.arima2010.small)

# Plot resudials
plot(resid(ETC.arima2010.small))

# No significance, good
par(mfrow = c(2, 1)) 
acf(resid(ETC.arima2010.small), 
	lag.max = 36,
	ylim = c(-0.1, 0.1), 
	lwd = 5, col = orange,
	na.action = na.pass)
pacf(resid(ETC.arima2010.small), 
	 lag.max = 36, 
	 lwd = 5, col = orange,
	 na.action = na.pass)
par(mfrow = c(1, 1))

# Residuals are definetly not correlated
Box.test(resid(BTC.arima510), type = "Ljung-Box", lag = 10)


