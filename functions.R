# This function scraps the OHLC historical crypto prices from www.coinmarketcap.com
# By the lecturer
getCryptoHistoricalPrice <- function(currency){
	paste0("https://coinmarketcap.com/currencies/",
		   currency,
		   "/historical-data/?start=20190101&end=21000101") %>%
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

# By the lecturer
plot_ACF_PACF_resids <- function(ARIMA_model) {
	
	par(mfrow = c(2, 1)) 
	acf(resid(ARIMA_model), 
		lag.max = 36,
		ylim = c(-0.1, 0.1), 
		lwd = 5, col = "dark green",
		na.action = na.pass)
	pacf(resid(ARIMA_model), 
		 lag.max = 36, 
		 lwd = 5, col = "dark green",
		 na.action = na.pass)
	par(mfrow = c(1, 1))
}

# By the lecturer
testdf <- function(variable, max.augmentations)
{
	
	require(fUnitRoots)
	require(lmtest)
	
	results_adf <- data.frame(augmentations = -1, adf = 0, p_adf = 0, bgodfrey = 0, p_bg = 0)
	variable <- coredata(variable[!is.na(variable)])
	
	for(augmentations in 0:max.augmentations)
	{
		df.test_ <- adfTest(variable, lags = augmentations, type = "c")
		df_ <- as.numeric(df.test_@test$statistic)
		p_adf <- as.numeric(df.test_@test$p.value)
		resids_ <- df.test_@test$lm$residuals
		bgtest_ <- bgtest(resids_~1, order = 1)
		bgodfrey <- bgtest_$statistic
		names(bgodfrey) <- NULL
		p_bg <- bgtest_$p.value
		
		results_adf <- rbind(results_adf, data.frame(augmentations = augmentations, adf = df_, p_adf = p_adf,
													 bgodfrey = bgodfrey, p_bg = p_bg))
		rm(df.test_, df_, resids_, bgtest_, bgodfrey, p_bg)
	}
	
	results_adf <- results_adf[results_adf$augmentations >= 0,]
	
	row.names(results_adf) <- NULL
	
	return(results_adf)
}	