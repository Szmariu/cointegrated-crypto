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
library(vars)
library(Metrics)

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
#getCryptoHistoricalPrice("bitcoin") %>% write_csv('data/btc.csv')
#getCryptoHistoricalPrice("ethereum") %>% write_csv('data/eth.csv')

btc <- read_csv('data/btc.csv')
eth <- read_csv('data/eth.csv')


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
 
data$Dbtc <- diff.xts(data$btc)
data$Deth <- diff.xts(data$eth)

# Drop the first row with NAs
data <- data %>% drop_na()

# Extract last 30 days for predictions
data_test <- data %>% tail(30)

# Remove them to avoid look ahead bias
data <- data %>% head(-30) # So usefull

###################################################
# ARIMA
###################################################

### Bitcoin ###
# Stationarity
testdf(variable = data$btc,
	   max.augmentations = 30)
# I(1) is enough
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

# I(o) would probably be ok
# But I'll use I(1) to make things more interesting
# And just to make sure 
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
auto.arima(data$eth,
		   d = 1,
		   max.p = 240,
		   max.q = 240,
		   stepwise = FALSE,
		   parallel = TRUE,
		   num.cores = 12)

# But let's try something bigger
ETH.arima5120 <- Arima(data$eth,  
					   order = c(5, 1, 20)  
)

# Many are significant
coeftest(ETH.arima5120)

# Make the smaller model
ETH.arima5120.small <- Arima(data$eth,  
							order = c(5, 1, 20),
							fixed = c(
								rep(0,3),
								NA,
								NA,
								rep(0, 3),
								NA,
								NA,
								rep(0, 3),
								NA,
								0,
								0,
								rep(0, 8),
								NA
							)
)

coeftest(ETH.arima5120.small)

# Plot resudials
plot(resid(ETH.arima5120.small))

# Some are on the verge, but it's ok
par(mfrow = c(2, 1)) 
acf(resid(ETH.arima5120.small), 
	lag.max = 36,
	ylim = c(-0.1, 0.1), 
	lwd = 5, col = orange,
	na.action = na.pass)
pacf(resid(ETH.arima5120.small), 
	 lag.max = 36, 
	 lwd = 5, col = orange,
	 na.action = na.pass)
par(mfrow = c(1, 1))

# Residuals are not correlated
Box.test(resid(ETH.arima5120.small), type = "Ljung-Box", lag = 10)


###################################################
# Forecasting with ARIMA
###################################################

# Bitcoin
forecast(BTC.arima2010.small, h = 30) %>%                  # Forecast
	{data.frame(f_mean  = as.numeric(.$mean),              # Make into a DF
			   f_lower = as.numeric(.$lower[, 2]),
			   f_upper = as.numeric(.$upper[, 2]))} %>%
	cbind(data_test) %>%                                   # Add the real data
	ggplot(aes(x = date)) +                                # Plot
		geom_line(aes(y = btc), color = purple) +
		geom_line(aes(y = f_mean), color = orange) +
		geom_line(aes(y = f_lower), color = 'red') +
		geom_line(aes(y = f_upper), color = 'red') +
		labs(title = "30 day prediction of BTC",
			 subtitle = 'Yellow is the prediction') +
		theme(
			text = element_text(family = "sans"), # Remove the font warnings
			plot.title = element_text(family = "sans")
		)

# Etherium
forecast(ETH.arima5120.small, h = 30) %>%                  # Forecast
	{data.frame(f_mean  = as.numeric(.$mean),              # Make into a DF
				f_lower = as.numeric(.$lower[, 2]),
				f_upper = as.numeric(.$upper[, 2]))} %>%
	cbind(data_test) %>%                                   # Add the real data
	ggplot(aes(x = date)) +                                # Plot
	geom_line(aes(y = eth), color = purple) +
	geom_line(aes(y = f_mean), color = orange) +
	geom_line(aes(y = f_lower), color = 'red') +
	geom_line(aes(y = f_upper), color = 'red') +
	labs(title = "30 day prediction of ETH",
		 subtitle = 'Yellow is the prediction') +
	theme(
		text = element_text(family = "sans"), # Remove the font warnings
		plot.title = element_text(family = "sans")
	)


###################################################
# Cointegration
###################################################

# BTC and ETH are I(1)
model.coint <- lm(btc ~ eth, 
				  data = data)

summary(model.coint)

# Testing stationarity of residuals 
# What is the proper ADF statistic? 

# Looks like I(1)
testdf(variable = residuals(model.coint), 
	   max.augmentations = 3)

# Add the lagged residuals
data$lresid <- lag.xts(residuals(model.coint))


model.ecm <- lm(btc ~ eth + lresid - 1,
				# -1 denotes a model without a constant
				data = data) 

# Looks like the are cointegrated quite nicely
summary(model.ecm)


# Both tests are significant
grangertest(btc ~ eth, 
			data = data,
			order = 5) 

grangertest(eth ~ btc, 
			data = data,
			order = 5)





###################################################
# VAR model
###################################################

# Lets proceed with 7
VARselect(data[ , c('btc', 'eth')], # input data for VAR
		  lag.max = 10)     # maximum lag

var.model <- VAR(data[ , c('btc', 'eth')], p = 7)

summary(var.model)

# On the verge of significance on the PACF
plot(var.model)

# On the verge of significance
serial.test(var.model)

# Not significant
serial.test(var.model, type = "BG")


# BTC has much stronger impact (maybe due to the diff in the values) 
plot( irf(var.model,
		  n.ahead = 36) )
plot( fevd(var.model,
		   n.ahead = 36))


###################################################
# VAR forecasting
###################################################

# Forecast for 30 days
var.forecast <- predict(var.model, n.ahead = 30, ci = 0.95) 

# Bitcoin
var.forecast[["fcst"]][["btc"]] %>%
	as.data.frame() %>%
	dplyr::select(f_mean = fcst,
		   		  f_lower = lower,
		          f_upper = upper) %>%
	cbind(data_test) %>%                                   # Add the real data
	ggplot(aes(x = date)) +                                # Plot
	geom_line(aes(y = btc), color = purple) +
	geom_line(aes(y = f_mean), color = orange) +
	geom_line(aes(y = f_lower), color = 'red') +
	geom_line(aes(y = f_upper), color = 'red') +
	labs(title = "30 day prediction of BTC",
		 subtitle = 'Yellow is the prediction') +
	theme(
		text = element_text(family = "sans"), # Remove the font warnings
		plot.title = element_text(family = "sans")
	)

# Etherium
var.forecast[["fcst"]][["eth"]] %>%
	as.data.frame() %>%
	dplyr::select(f_mean = fcst,
				  f_lower = lower,
				  f_upper = upper) %>%
	cbind(data_test) %>%                                   # Add the real data
	ggplot(aes(x = date)) +                                # Plot
	geom_line(aes(y = eth), color = purple) +
	geom_line(aes(y = f_mean), color = orange) +
	geom_line(aes(y = f_lower), color = 'red') +
	geom_line(aes(y = f_upper), color = 'red') +
	labs(title = "30 day prediction of ETH",
		 subtitle = 'Yellow is the prediction') +
	theme(
		text = element_text(family = "sans"), # Remove the font warnings
		plot.title = element_text(family = "sans")
	)


###################################################
# Forecast comparison
###################################################

# Arima 
# Bitcoin
forecast.arima.btc <- forecast(BTC.arima2010.small, h = 30) %>% 
	.$mean %>%
	as.vector()

# Etherium
forecast.arima.eth <- forecast(ETH.arima5120.small, h = 30) %>% 
	.$mean %>%
	as.vector()

# VAR
# Bitcoin
forecast.var.btc <- var.forecast[["fcst"]][["btc"]] %>%
	as.data.frame() %>%
	.$fcst


# Etherium
forecast.var.eth <- var.forecast[["fcst"]][["eth"]] %>%
	as.data.frame() %>%
	.$fcst

forecast.comparison <- data.frame(
	btc_real = data_test$btc,
	btc_arima = forecast.arima.btc,
	btc_var = forecast.var.btc,
	eth_real = data_test$eth,
	eth_arima = forecast.arima.eth,
	eth_var = forecast.var.eth
)

data.frame(
	ARIMA_BTC = smape(forecast.comparison$btc_real, forecast.comparison$btc_arima),
	ARIMA_ETH = smape(forecast.comparison$eth_real, forecast.comparison$eth_arima),
	VAR_BTC   = smape(forecast.comparison$btc_real, forecast.comparison$btc_var),
	VAR_ETH   = smape(forecast.comparison$eth_real, forecast.comparison$eth_var)
)








