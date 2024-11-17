library(readxl)
library(xts)
library(forecast)
library(tseries)
library(moments)
library(astsa)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(lmtest)
library(MuMIn)


#Extracting the time-series data for the FTSE 100 stocks. 

stockData <- read_excel("ADD-YOUR-PATH-TO-THE-DATASET-HERE.xlsx", skip = 2, col_names = TRUE)

#Check the stockData Info

is.xts(stockData) # Is it a time series?
print(stockData)
str(stockData)

##################################
# Cleaning the Data for Analysis
###############################

# the first column needs to be called 'Dates'

colnames(stockData)[1] <- 'Dates'

#Converting the dates into date object
stockData$Dates <- as.Date(as.numeric(stockData$Dates), origin = "1899-12-30")

#Check
print(stockData$Dates)

#omits the 'NA' row in dates
stockData_Clean <- stockData[!is.na(stockData$Dates), ]

#Check the data
head(stockData)
head(stockData_Clean)
dates <- stockData_Clean$Dates

#Checking if the data is correct for the analysis

print(stockData_Clean)

### Specify the Stocks to analyse by Sector 

foodanddrugsStocks <- c("OCDO LN Equity", "MKS LN Equity", "SBRY LN Equity", "TSCO LN Equity")

miningStocks <- c("AAL LN Equity", "ANTO LN Equity", "FRES LN Equity", "GLEN LN Equity", "RIO LN Equity")

realestateTrustsStocks <- c("LAND LN Equity", "SGRO LN Equity", "UTG LN Equity")

# Combing the sectors into one variable

FTSE.stocks <- c(foodanddrugsStocks, miningStocks, realestateTrustsStocks)

# Creating an empty list to store time series (xts) objects

stockXTSList <- list()

# Loop to create xts objects for each stock 

for (ticker in FTSE.stocks) {
  if (ticker %in% colnames(stockData_Clean)) {

    # Create the xts object making sure the columns are numeric
    xtsData <- xts(x = as.numeric(stockData_Clean[[ticker]]), order.by = dates)
    # Add the xts object to the list, naming it by the ticker
    stockXTSList[[ticker]] <- xtsData
  }
}

# Checking if a stock is an xts object. If 'TRUE', we know that it has worked.
is.xts(stockXTSList[["MKS LN Equity"]])

##########################
# Stationarity test 
##########################

plot(stockXTSList[["MKS LN Equity"]])   # Plots the time series for MKS

# Plotting the ACF and PACF to see if the data looks stationary
acf2(stockXTSList[["MKS LN Equity"]], main ="Marks & Spencer ACF and PACF plots (pre-transformation)" )

#the acf and adf shows that it is not stationary

# ADF test to confirm non-stationarity hence we need to make it stationary
adf.test(stockXTSList[["MKS LN Equity"]])

# This is done for MKS as an example but it holds for all stocks by replacing the ticker. 


###########################
# Making the data stationary  
###########################

stationaryStockList <- list() # creates list to store stationary data

#Log and difference the data to make it stationary - turning the data into log-returns/returns

for (ticker in names(stockXTSList)){
  stationary_series <- diff(log(stockXTSList[[ticker]]))
  stationaryStockList[[ticker]] <- stationary_series
}

# Now, let's test if this data is stationary, noting we lose the first observation after we difference. 

adf.test(na.omit(stationaryStockList[["OCDO LN Equity"]])) # According to the ADF, the data is stationary
acf2(na.omit(stationaryStockList[["OCDO LN Equity"]]), main = "Ocado Group ACF and PACF plots (Post-transformation)")


############################################################
# Graphically the stationary data vs the non-stationary data 
############################################################

# Lets further motivate the reasoning of our transformation. By plotting the data and distribution before and after
# the transformation, we can identify features of a non-stationary series which is subsequently dealt with. 

#### For REIT Industry 

# Assigning the original data to variables -> pre-transformed data
LAND_OD <- stockXTSList[["LAND LN Equity"]]
SGRO_OD <- stockXTSList[["SGRO LN Equity"]]
UTG_OD <- stockXTSList[["UTG LN Equity"]]


# Checking the length of the vectors
length(LAND_OD)
length(SGRO_OD)
length(UTG_OD)

# Assigning the new transformed data to variables

LAND_LD <- stationaryStockList[["LAND LN Equity"]]
SGRO_LD <- stationaryStockList[["SGRO LN Equity"]]
UTG_LD <- stationaryStockList[["UTG LN Equity"]]

# Set up the layout for a 2x2 plotting window
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Plot 1: Time-Series of pre-transformed data (Asset Prices), with time-series dates as the x-axis. 

plot(dates, LAND_OD, type = "l", col = "blue", ylim = c(0, max(LAND_OD, SGRO_OD, UTG_OD)), xlab = "Date", ylab = "Asset Prices", main = "Asset Prices: Time-Series")
lines(dates, SGRO_OD, col = "red")
lines(dates, UTG_OD, col = "orange")
legend("bottomleft", legend = c("LAND", "SGRO", "UTG"), col = c("blue", "red", "orange"), lty = 1)

# Plot 2: Histogram/Distribution of pre-transformed data

# Creating histograms
dens_land = density(na.omit(LAND_OD))  # omitting NA values so the code runs. 
dens_sgro = density(na.omit(SGRO_OD))
dens_utg = density(na.omit(UTG_OD))
# Find the range of all densities to correctly format the figure 
xlim_range = range(dens_land$x, dens_sgro$x, dens_utg$x)
ylim_range = range(dens_land$y, dens_sgro$y, dens_utg$y)

# Plotting code with correct format
plot(dens_land, xlim = xlim_range, ylim = ylim_range, main = "Asset Prices: Distribution", xlab = "Asset Prices", ylab = "Distribution")
# Use polygon to create filled density plots
polygon(dens_land, col = rgb(0, 0, 1, 0.5), border = "blue")
polygon(dens_sgro, col = rgb(1, 0, 0, 0.5), border = "red")
polygon(dens_utg, col = rgb(1, 0.5, 0, 0.5), border = "orange")
# Add a legend
legend("topright", legend = c("LAND", "SGRO", "UTG"), col = c("blue", "red", "orange"), lty = 1, fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5), rgb(1, 0.5, 0, 0.5)))

# Plot 3: Time-Series of transformed data - Asset Log Returns

# This helps correct the y-axis scale
max_log_return <- max(c(LAND_LD, SGRO_LD, UTG_LD), na.rm = TRUE)

plot(dates, LAND_LD, type = "l", col = "blue", ylim = c(min(LAND_LD, SGRO_LD, UTG_LD, na.rm = TRUE), max_log_return), xlab = "Date", ylab = "Asset Log Returns", main = "Asset Log Returns: Time Series")
lines(dates, SGRO_LD, col = "red")
lines(dates, UTG_LD, col = "orange")
legend("bottomleft", legend = c("LAND", "SGRO", "UTG"), col = c("blue", "red", "orange"), lty = 1)

# Plot 4: Density Plot of transformed data
# Creating histograms
dens_land <- density(na.omit(LAND_LD))
dens_sgro <- density(na.omit(SGRO_LD))
dens_utg <- density(na.omit(UTG_LD))

plot(dens_land, xlim = c(-0.15, 0.15), ylim = c(0,30), col = "blue", xlab = "Asset Log Returns", ylab = "Distribution", main = "Asset Log Returns: Distribution")
polygon(dens_land, col = rgb(0, 0, 1, 0.3), border = "blue")
lines(dens_sgro, col = "red")
polygon(dens_sgro, col = rgb(1, 0, 0, 0.3), border = "red")
lines(dens_utg, col = "orange")
polygon(dens_utg, col = rgb(1, 0.5, 0, 0.3), border = "orange")
legend("topright", legend = c("LAND", "SGRO", "UTG"), col = c("blue", "red", "orange"), lty = 1, fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5), rgb(1, 0.5, 0, 0.5)))




#### For Mining Industry - following the same steps as before. 

# Assigning the original data to variables -> pre-transformed data
AAL_OD <- stockXTSList[["AAL LN Equity"]]
ANTO_OD <- stockXTSList[["ANTO LN Equity"]]
FRES_OD <- stockXTSList[["FRES LN Equity"]]
GLEN_OD <- stockXTSList[["GLEN LN Equity"]]
RIO_OD <- stockXTSList[["RIO LN Equity"]]

length(AAL_OD)
length(ANTO_OD)
length(FRES_OD)
length(GLEN_OD)
length(RIO_OD)

# Assigning the new transformed data to variables

AAL_LD <- stationaryStockList[["AAL LN Equity"]]
ANTO_LD <- stationaryStockList[["ANTO LN Equity"]]
FRES_LD <- stationaryStockList[["FRES LN Equity"]]
GLEN_LD <- stationaryStockList[["GLEN LN Equity"]]
RIO_LD <- stationaryStockList[["RIO LN Equity"]]

# Set up the layout for a 2x2 plotting window
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Plot 1: Time-Series of pre-transformed data (Asset Prices)
plot(dates, AAL_OD, type = "l", col = "blue", ylim = c(0, max(AAL_OD, ANTO_OD, FRES_OD, GLEN_OD, RIO_OD)), xlab = "Date", ylab = "Asset Prices", main = "Asset Prices: Time-Series")
lines(dates, ANTO_OD, col = "red")
lines(dates, FRES_OD, col = "orange")
lines(dates, GLEN_OD, col = "pink")
lines(dates, RIO_OD, col = "purple")
legend("topleft", legend = c("AAL", "ANTO", "FRES", "GLEN", "RIO"), col = c("blue", "red", "orange", "pink", "purple"), lty = 1, cex = 0.75, pt.cex = 0.75, lwd = 0.5)

# Plot 2: Histogram/Distribution of Asset Prices
# Creating histograms
dens_aal = density(na.omit(AAL_OD))  
dens_anto = density(na.omit(ANTO_OD))
dens_fres = density(na.omit(FRES_OD))
dens_glen = density(na.omit(GLEN_OD))
dens_rio = density(na.omit(RIO_OD))
# Find the range of all densities to set axis 
xlim_range = range(dens_aal$x, dens_anto$x, dens_fres$x, dens_glen$x, dens_rio$x)
ylim_range = range(dens_aal$y, dens_anto$y, dens_fres$y, dens_glen$y, dens_rio$y)

plot(dens_aal, xlim = xlim_range, ylim = ylim_range, main = "Asset Prices: Distribution", xlab = "Asset Prices", ylab = "Distribution") 
# Use polygon to create filled density plots
polygon(dens_aal, col = rgb(0, 0, 1, 0.5), border = "blue")
polygon(dens_anto, col = rgb(1, 0, 0, 0.5), border = "red")
polygon(dens_fres, col = rgb(1, 0.5, 0, 0.5), border = "orange")
polygon(dens_glen, col = rgb(1, 0.5, 0, 0.5), border = "pink")
polygon(dens_rio, col = rgb(0.5, 0, 0.5, 0.5), border = "purple")
legend("topright", legend = c("AAL", "ANTO", "FRES", "GLEN", "RIO"), col = c("blue", "red", "orange", "pink", "purple"), lty = 1, fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5), rgb(1, 0.5, 0, 0.5), rgb(1, 0.5, 0.5, 0.5), rgb(0.5, 0, 0.5, 0.5)))

# Plot 3: Time-Series of transformed data (Log Returns)
max_log_return <- max(c(AAL_LD, ANTO_LD, FRES_LD, GLEN_LD, RIO_LD), na.rm = TRUE)
plot(dates, AAL_LD, type = "l", col = "blue", ylim = c(min(AAL_LD, ANTO_LD, FRES_LD, GLEN_LD, RIO_LD, na.rm = TRUE), max_log_return), xlab = "Date", ylab = "Asset Log Returns", main = "Asset Log Returns: Time Series")
lines(dates, ANTO_LD, col = "red")
lines(dates, FRES_LD, col = "orange")
lines(dates, GLEN_LD, col = "pink")
lines(dates, RIO_LD, col = "purple")
legend("bottomleft", legend = c("AAL", "ANTO", "FRES", "GLEN", "RIO"), col = c("blue", "red", "orange", "pink", "purple"), lty = 1, cex = 0.75, pt.cex = 0.75, lwd = 0.5)

# Plot 4: Density Plot of Asset Log Returns
dens_aal <- density(na.omit(AAL_LD))
dens_anto <- density(na.omit(ANTO_LD))
dens_fres <- density(na.omit(FRES_LD))
dens_glen <- density(na.omit(GLEN_LD))
dens_rio <- density(na.omit(RIO_LD))

plot(dens_aal, xlim = c(-0.15, 0.15), ylim = c(0,25), col = "blue", xlab = "Asset Log Returns", ylab = "Distribution", main = "Asset Log Returns: Distribution")
polygon(dens_aal, col = rgb(0, 0, 1, 0.3), border = "blue")
lines(dens_anto, col = "red")
polygon(dens_anto, col = rgb(1, 0, 0, 0.3), border = "red")
lines(dens_fres, col = "orange")
polygon(dens_fres, col = rgb(1, 0.5, 0, 0.3), border = "orange")
lines(dens_glen, col = "pink")
polygon(dens_glen, col = rgb(1, 0.5, 0, 0.3), border = "pink")
lines(dens_rio, col = "purple")
polygon(dens_rio, col = rgb(0.5, 0, 0.5, 0.3), border = "purple")
legend("topright", legend = c("AAL", "ANTO", "FRES", "GLEN", "RIO"), col = c("blue", "red", "orange", "pink", "purple"), lty = 1, fill = c(rgb(0, 0, 1, 0.3), rgb(1, 0, 0, 0.3), rgb(1, 0.5, 0, 0.3), rgb(1, 0.5, 0.5, 0.3), rgb(0.5, 0, 0.5, 0.3)))


### For the food & Retail industry 

# Assigning the original data to variables -> pre-transformed data
OCDO_OD <- stockXTSList[["OCDO LN Equity"]]
MKS_OD <- stockXTSList[["MKS LN Equity"]]
SBRY_OD <- stockXTSList[["SBRY LN Equity"]]
TSCO_OD <- stockXTSList[["TSCO LN Equity"]]

length(OCDO_OD)
length(MKS_OD)
length(SBRY_OD)
length(TSCO_OD)

# Assigning the new transformed data to variables

OCDO_LD <- stationaryStockList[["OCDO LN Equity"]]
MKS_LD <- stationaryStockList[["MKS LN Equity"]]
SBRY_LD <- stationaryStockList[["SBRY LN Equity"]]
TSCO_LD <- stationaryStockList[["TSCO LN Equity"]]


# Set up the layout for a 2x2 plotting window
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Plot 1: Time-Series of pre-transformed data (Asset Prices)
plot(dates, OCDO_OD, type = "l", col = "blue", ylim = c(0, max(OCDO_OD, MKS_OD, SBRY_OD, TSCO_OD)), xlab = "Date", ylab = "Asset Prices", main = "Asset Prices: Time-Series")
lines(dates, MKS_OD, col = "red")
lines(dates, SBRY_OD, col = "orange")
lines(dates, TSCO_OD, col = "#f46a9b")
legend("topleft", legend = c("OCDO", "MKS", "SBRY", "TSCO"), col = c("blue", "red", "orange", "#f46a9b"), lty = 1)

# Plot 2: Histogram/Distribution of Asset Prices
# Creating histograms
dens_ocdo = density(na.omit(OCDO_OD)) 
dens_mks = density(na.omit(MKS_OD))
dens_sbry = density(na.omit(SBRY_OD))
dens_tsco = density(na.omit(TSCO_OD))
# Find the range of all densities to make the axis look good
xlim_range = range(dens_ocdo$x, dens_mks$x, dens_sbry$x, dens_tsco$x)
ylim_range = range(dens_ocdo$y, dens_mks$y, dens_sbry$y, dens_tsco$y)

plot(dens_ocdo, xlim = xlim_range, ylim = ylim_range, main = "Asset Prices: Distribution", xlab = "Asset Prices", ylab = "Distribution")
# Use polygon to create filled density plots
polygon(dens_ocdo, col = rgb(0, 0, 1, 0.5), border = "blue")
polygon(dens_mks, col = rgb(1, 0, 0, 0.5), border = "red")
polygon(dens_sbry, col = rgb(1, 0.5, 0, 0.5), border = "orange")
polygon(dens_tsco, col = rgb(244/255, 106/255, 155/255, 0.5), border = "#f46a9b")
legend("topright", legend = c("OCDO", "MKS", "SBRY", "TSCO"), col = c("blue", "red", "orange", "#f46a9b"), lty = 1, fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5), rgb(1, 0.5, 0, 0.5), rgb(244/255, 106/255, 155/255, 0.5)))

# Plot 3: Time-Series of transformed data (Log Returns)
max_log_return <- max(c(OCDO_LD, MKS_LD, SBRY_LD, TSCO_LD), na.rm = TRUE)
plot(dates, OCDO_LD, type = "l", col = "blue", ylim = c(min(OCDO_LD, MKS_LD, SBRY_LD, TSCO_LD, na.rm = TRUE), max_log_return), xlab = "Date", ylab = "Asset Log Returns", main = "Asset Log Returns: Time Series")
lines(dates, MKS_LD, col = "red")
lines(dates, SBRY_LD, col = "orange")
lines(dates, TSCO_LD, col = "#f46a9b")
legend("topright", legend = c("OCDO", "MKS", "SBRY", "TSCO"), col = c("blue", "red", "orange", "#f46a9b"), lty = 1)

# Plot 4: Density Plot of Asset Log Returns
dens_ocdo <- density(na.omit(OCDO_LD))
dens_mks <- density(na.omit(MKS_LD))
dens_sbry <- density(na.omit(SBRY_LD))
dens_tsco <- density(na.omit(TSCO_LD))

plot(dens_ocdo, xlim = c(-0.15, 0.15), ylim = c(0, 35),  col = "blue", xlab = "Asset Log Returns", ylab = "Distribution", main = "Asset Log Returns: Distribution")
polygon(dens_ocdo, col = rgb(0, 0, 1, 0.3), border = "blue")
lines(dens_mks, col = "red")
polygon(dens_mks, col = rgb(1, 0, 0, 0.3), border = "red")
lines(dens_sbry, col = "orange")
polygon(dens_sbry, col = rgb(1, 0.5, 0, 0.3), border = "orange")
lines(dens_tsco, col = "#f46a9b")
polygon(dens_tsco, col = rgb(244/255, 106/255, 155/255, 0.3), border = "#f46a9b")
legend("topright", legend = c("OCDO", "MKS", "SBRY", "TSCO"), col = c("blue", "red", "orange", "#f46a9b"), lty = 1, fill = c(rgb(0, 0, 1, 0.5), rgb(1, 0, 0, 0.5), rgb(1, 0.5, 0, 0.5), rgb(244/255, 106/255, 155/255, 0.5)))



####################
# Summary Statistics 
####################

# This section finds the summary stats of the transformed, stationary data before we fit the ARMA models. 

# Create a table for the data 
stats_table <- data.frame(
  Stock = character(),Mean = numeric(),SD = numeric(),Skewness = numeric(),stringsAsFactors = FALSE)

# Loop through each stock and calculate statistics
for(ticker in names(stationaryStockList)) {
  # Extract the stock prices for the given stock ticker
  stock_returns <- as.numeric(na.omit(stationaryStockList[[ticker]]))
  
  # Calculate the mean, skewness, s.d, 1-day-ac
  stock_mean <- mean(stock_returns, na.rm = TRUE)
  stock_sd <- sd(stock_returns, na.rm = TRUE)
  stock_skewness <- skewness(stock_returns, na.rm = TRUE)
  stock_autocorrelation <- cor(stock_returns[-length(stock_returns)], stock_returns[-1], use = "complete.obs")


  # Append the statistics to the data frame
  stats_table <- rbind(stats_table, data.frame( Stock = ticker, Mean = stock_mean, SD = stock_sd, Skewness = stock_skewness, Autocorrelation = stock_autocorrelation))
}

print(stats_table)



###########################################################
# Finding the best ARMA model based on the lowest AIC test
###########################################################

# Creating a list to store the ARMA models 
bestARMAList <- list()

# Loop over all stocks to fit ARMA models that minimise the AIC 
# Limiting the order to a max of 3 to avoid complex models. 

for (ticker in names(stationaryStockList)){
  series <- na.omit(stationaryStockList[[ticker]])
  
  # Creates vectors that get updated after each model iteration below
  final_aic <- Inf
  final_order <- c(0,0,0)
  final_arma <- NULL
  
  # Finding the order of the AR and MA components 
  for (i in 0:3) for (j in 0:3) {
    current_model <- arima(series, order = c(i,0,j))
    current_aic <- AIC(current_model)
    if (current_aic < final_aic) {
      final_aic <- current_aic
      final_order <- c(i, 0, j)
      final_arma <- arima(series, order=final_order)
    
    }
  }
  
  # Stores the best ARMA orders into the list. 
  bestARMAList[[ticker]] <- list(order = final_order, aic = final_aic, model = final_arma)
}

# Loop through the list of best models to print the order for each ticker
for(ticker in names(bestARMAList)) {
  
  model <- bestARMAList[[ticker]][["order"]]
  
  # Use arimaorder() to extract the order of the model
  arimaOrder <- model
  print(paste0('The model specification of ', ticker, ': ', toString(arimaOrder)))
}


###############
# Testing the models - Augmented Dickey Fuller Test and Inverse Roots to see that the models are valid 

# Food & Drug Retailers

TSCO_model <- bestARMAList[["TSCO LN Equity"]][["model"]]
TSCO_fitted_ARMA <- fitted(TSCO_model)
adf.test(TSCO_fitted_ARMA)
autoplot(TSCO_model)

MKS_model <- bestARMAList[["MKS LN Equity"]][["model"]]
MKS_fitted_ARMA <- fitted(MKS_model)
adf.test(MKS_fitted_ARMA)
autoplot(MKS_model)

SBRY_model <- bestARMAList[["SBRY LN Equity"]][["model"]]
SBRY_fitted_ARMA <- fitted(SBRY_model)
adf.test(SBRY_fitted_ARMA)
autoplot(SBRY_model)

OCDO_model <- bestARMAList[["OCDO LN Equity"]][["model"]]
OCDO_fitted_ARMA <- fitted(OCDO_model)
adf.test(OCDO_fitted_ARMA)
autoplot(OCDO_model)

# REITs

SGRO_model <- bestARMAList[["SGRO LN Equity"]][["model"]]
SGRO_fitted_ARMA <- fitted(SGRO_model)
adf.test(SGRO_fitted_ARMA)
autoplot(SGRO_model)

LAND_model <- bestARMAList[["LAND LN Equity"]][["model"]]
LAND_fitted_ARMA <- fitted(LAND_model)
adf.test(LAND_fitted_ARMA)
autoplot(LAND_model)

UTG_model <- bestARMAList[["UTG LN Equity"]][["model"]]
UTG_fitted_ARMA <- fitted(UTG_model)
adf.test(UTG_fitted_ARMA)
autoplot(UTG_model)


# Mining

AAL_model <- bestARMAList[["AAL LN Equity"]][["model"]]
AAL_fitted_ARMA <- fitted(AAL_model)
adf.test(AAL_fitted_ARMA)
autoplot(AAL_model)

ANTO_model <- bestARMAList[["ANTO LN Equity"]][["model"]]
ANTO_fitted_ARMA <- fitted(ANTO_model)
adf.test(ANTO_fitted_ARMA)
autoplot(ANTO_model)

GLEN_model <- bestARMAList[["GLEN LN Equity"]][["model"]]
GLEN_fitted_ARMA <- fitted(GLEN_model)
adf.test(GLEN_fitted_ARMA)
autoplot(GLEN_model)

RIO_model <- bestARMAList[["RIO LN Equity"]][["model"]]
RIO_fitted_ARMA <- fitted(RIO_model)
adf.test(RIO_fitted_ARMA)
autoplot(RIO_model)

FRES_model <- bestARMAList[["FRES LN Equity"]][["model"]]
FRES_fitted_ARMA <- fitted(FRES_model)
adf.test(FRES_fitted_ARMA)
autoplot(FRES_model)



#######################################################################

# Now, let's plot the fitted values from ARMA models on actual data
# This will help visualise how well the data has been modelled. 

#### For Food & Retail

# Original Transformed data assigned to variables 

diff_data_tsco <- na.omit(stationaryStockList[["TSCO LN Equity"]])
diff_data_mks <- na.omit(stationaryStockList[["MKS LN Equity"]])
diff_data_sbry <- na.omit(stationaryStockList[["SBRY LN Equity"]])
diff_data_ocdo <- na.omit(stationaryStockList[["OCDO LN Equity"]])

length(diff_data_tsco)
length(diff_data_mks)
length(diff_data_sbry)
length(diff_data_ocdo)

# Dates are 1319 in length but we lose the first observation after differencing. 
#Hence, I remove the first date. 

Adjusted_dates <- dates[-1]
length(Adjusted_dates)

# layout for a 2x2 plotting window
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))

# Plot 1: Fitted ARMA Model - TSCO
plot(Adjusted_dates, diff_data_tsco, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for TSCO"))
lines(Adjusted_dates, TSCO_fitted_ARMA, col = "red")
legend("bottomleft", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Plot 2: Fitted ARMA Model - MKS
plot(Adjusted_dates, diff_data_mks, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for MKS"))
lines(Adjusted_dates, MKS_fitted_ARMA, col = "red")
legend("topright", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Plot 3: Fitted ARMA Model - SBRY
plot(Adjusted_dates, diff_data_sbry, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for SBRY"))
lines(Adjusted_dates, SBRY_fitted_ARMA, col = "red")
legend("bottomleft", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Plot 4: Fitted ARMA Model - OCDO
plot(Adjusted_dates, diff_data_ocdo, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for OCDO"))
lines(Adjusted_dates, OCDO_fitted_ARMA, col = "red")
legend("topright", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

#### For Mining

# Original Transformed data assigned to variables 

diff_data_aal<- na.omit(stationaryStockList[["AAL LN Equity"]])
diff_data_glen <- na.omit(stationaryStockList[["GLEN LN Equity"]])
diff_data_rio <- na.omit(stationaryStockList[["RIO LN Equity"]])
diff_data_anto <- na.omit(stationaryStockList[["ANTO LN Equity"]])
diff_data_fres <- na.omit(stationaryStockList[["FRES LN Equity"]])

length(diff_data_aal)
length(diff_data_glen)
length(diff_data_rio)
length(diff_data_anto)
length(diff_data_fres)


# We have five models for the mining industry so the layout needs to be adjusted
layout_matrix <- matrix(c(1, 2, 3, 4, 5, 5), byrow = TRUE, nrow = 3)

# Apply the layout
layout(layout_matrix)

# Plot 1: Fitted Model - AAL
plot(Adjusted_dates, diff_data_aal, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for AAL"))
lines(Adjusted_dates, AAL_fitted_ARMA, col = "red")
legend("bottomleft", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Plot 2: Fitted Model - ANTO
plot(Adjusted_dates, diff_data_anto, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for ANTO"))
lines(Adjusted_dates, ANTO_fitted_ARMA, col = "red")
legend("topleft", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Plot 3: Fitted Model - GLEN
plot(Adjusted_dates, diff_data_glen, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for GLEN"))
lines(Adjusted_dates, GLEN_fitted_ARMA, col = "red")
legend("bottomleft", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Plot 4: Fitted Model - RIO
plot(Adjusted_dates, diff_data_rio, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for RIO"))
lines(Adjusted_dates, RIO_fitted_ARMA, col = "red")
legend("topleft", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Plot 5: Fitted Model - FRES
plot(Adjusted_dates, diff_data_fres, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for FRES"))
lines(Adjusted_dates, FRES_fitted_ARMA, col = "red")
legend("bottomleft", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)


#### For REITS

# Original Transformed data assigned to variables 

diff_data_sgro <- na.omit(stationaryStockList[["SGRO LN Equity"]])
diff_data_land <- na.omit(stationaryStockList[["LAND LN Equity"]])
diff_data_utg <- na.omit(stationaryStockList[["UTG LN Equity"]])

length(diff_data_sgro)
length(diff_data_land)
length(diff_data_utg)

layout_matrix <- matrix(c(1, 2, 3, 3), byrow = TRUE, nrow = 2)

# Apply the layout
layout(layout_matrix)

# Plot 1: Fitted Model - SGRO
plot(Adjusted_dates, diff_data_sgro, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for SGRO"))
lines(Adjusted_dates, SGRO_fitted_ARMA, col = "red")
legend("bottomleft", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Plot 2: Fitted Model - LAND
plot(Adjusted_dates, diff_data_land, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for LAND"))
lines(Adjusted_dates, LAND_fitted_ARMA, col = "red")
legend("topright", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)

# Plot 3: Fitted Model - UTG
plot(Adjusted_dates, diff_data_utg, type = "l", col = "blue", xlab = "Date", ylab = "Value", main = paste("Fitted ARMA Model vs. Original Series for UTG"))
lines(Adjusted_dates, UTG_fitted_ARMA, col = "red")
legend("bottomleft", legend = c("Original Returns (Differenced)", "Fitted Returns"), col = c("blue", "red"), lty = 1, cex = 0.8)


#################################
# Printing the Model Coefficients 
#################################

# I loop through the list of ARMA models to print the coefficients and their statistical significance for each ticker

for(ticker in names(bestARMAList)) {
  
  # Extract the ARMA model for the current ticker
  model <- bestARMAList[[ticker]][["model"]]
  
  # Use coeftest to get the statistical summary of the model's coefficients
  coefTestResults <- coeftest(model)
  
  # Print the ticker symbol
  cat("Coefficients and their statistical significance for", ticker, ":\n")
  
  # Print the results
  print(coefTestResults)
}

###############################
# Testing the model Assumptions 
###############################

# Loop to test that the models residuals follow a white-noise process

for(ticker in names(bestARMAList)) {
  model <- bestARMAList[[ticker]][["model"]]
  residuals <- residuals(model)
  
  # Applying the Ljung-Box test to the residuals
  lb_test <- Box.test(residuals, lag = log(length(residuals)), type = "Ljung-Box")
  
  # Printing the results
  cat("Ljung-Box test for", ticker, ":\n")
  print(lb_test)
}


# Visualise the models residuals 

#Food & Drug Sector
checkresiduals(bestARMAList[["OCDO LN Equity"]][["model"]])
checkresiduals(bestARMAList[["MKS LN Equity"]][["model"]])
checkresiduals(bestARMAList[["SBRY LN Equity"]][["model"]])
checkresiduals(bestARMAList[["TSCO LN Equity"]][["model"]])


# Mining Sector 
checkresiduals(bestARMAList[["AAL LN Equity"]][["model"]])
checkresiduals(bestARMAList[["ANTO LN Equity"]][["model"]])
checkresiduals(bestARMAList[["FRES LN Equity"]][["model"]])
checkresiduals(bestARMAList[["GLEN LN Equity"]][["model"]])
checkresiduals(bestARMAList[["RIO LN Equity"]][["model"]])

#REITs Sector 
checkresiduals(bestARMAList[["LAND LN Equity"]][["model"]])
checkresiduals(bestARMAList[["SGRO LN Equity"]][["model"]])
checkresiduals(bestARMAList[["UTG LN Equity"]][["model"]])

# Testing the models fit using Mean Squared Error (MSE) and Mean Absolute Error (MAE) 

# Initialize lists to store MSE and MAE values
MSE_values <- list()
MAE_values <- list()

# Loop through each model, calculate residuals, and compute MSE and MAE
for(ticker in names(bestARMAList)) {
  # Extract the fitted ARMA model
  model <- bestARMAList[[ticker]][["model"]]
  
  # Calculate the residuals from the model
  residuals <- residuals(model)
  
  # Calculate and store the MSE and MAE for the residuals of the current model
  MSE_values[[ticker]] <- mean(residuals^2, na.rm = TRUE)
  MAE_values[[ticker]] <- mean(abs(residuals), na.rm = TRUE)
}

#print the MSE and MAE values for each ticker
for(ticker in names(bestARMAList)) {
  cat("MSE for", ticker, ":", MSE_values[[ticker]], "\n")
  cat("MAE for", ticker, ":", MAE_values[[ticker]], "\n\n")
}
