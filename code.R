### Preparation
# Libraries
library(tidyverse)
library(ggplot2) # nice plots
library(ggthemes) 
library(ggtech) # For the airbnb theme

# For better plotting
theme_set(theme_airbnb_fancy())
orange = "#FFB400"
purple = "#7B0051"

# Fix for the month shorthands
Sys.setlocale("LC_ALL","English")

# This function scraps the OHLC historical crypto prices from www.coinmarketcap.com
getCryptoHistoricalPrice <- function(currency){
	paste0("https://coinmarketcap.com/currencies/",
		   currency,
		   "/historical-data/?start=20170101&end=21000101") %>%
		xml2::read_html() %>%
		rvest::html_table(.) %>%
		.[[3]] %>% 
		as_tibble() %>%
		rename(Open  = `Open*`,
			   Close = `Close**`,
			   MarketCap = `Market Cap`) %>%
		mutate(Date = as.Date(Date, format = "%b %d, %Y")) %>%
		mutate_if(is.character, function(x) as.numeric(gsub(",", "", x))) %>%
		arrange(Date) %>%
		return()
}

### Data downloading
btc <- getCryptoHistoricalPrice("bitcoin")
eth <- getCryptoHistoricalPrice("ethereum")

### Basic ETA
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
