# =============================================================================
# CARBON FACTOR (CF_t) - BROWN vs GREEN STOCK PORTFOLIO
#
# PROCESS:
# 1. Download DAILY adjusted closing prices
# 2. Calculate DAILY LOG RETURNS
# 3. Convert daily log returns into MONTHLY LOG RETURNS
# 4. Calculate WAR (Weighted Average Return)
# 5. Calculate Carbon Factor:
#       CF_t = WAR_Brown - WAR_Green
# 6. Stationarity tests on RETURNS
# =============================================================================


# -----------------------------------------------------------------------------
# 0. INSTALL AND LOAD PACKAGES
# -----------------------------------------------------------------------------

packages <- c("quantmod", "xts", "zoo", "tseries")

new_packages <- packages[!(packages %in%
                             installed.packages()[, "Package"])]

if(length(new_packages) > 0){
  install.packages(new_packages)
}

library(quantmod)
library(xts)
library(zoo)
library(tseries)


# -----------------------------------------------------------------------------
# 1. STOCK LIST
# -----------------------------------------------------------------------------

# 15 Brown stocks
brown_tickers <- c(
  "COALINDIA.NS",
  "NMDC.NS",
  "ONGC.NS",
  "IOC.NS",
  "BPCL.NS",
  "HINDPETRO.NS",
  "OIL.NS",
  "TATASTEEL.NS",
  "JSWSTEEL.NS",
  "SAIL.NS",
  "ULTRACEMCO.NS",
  "SHREECEM.NS",
  "AMBUJACEM.NS",
  "HINDALCO.NS",
  "VEDL.NS"
)


# 15 Green stocks
green_tickers <- c(
  "NHPC.NS",
  "SJVN.NS",
  "NTPC.NS",
  "TATAPOWER.NS",
  "ADANIGREEN.NS",
  "TORNTPOWER.NS",
  "SUZLON.NS",
  "INOXWIND.NS",
  "POWERGRID.NS",
  "SIEMENS.NS",
  "ABB.NS",
  "M&M.NS",
  "CESC.NS",
  "HAVELLS.NS",
  "IEX.NS"
)


# -----------------------------------------------------------------------------
# 2. DATE RANGE
# -----------------------------------------------------------------------------

start_date <- as.Date("2018-07-01")
end_date   <- as.Date("2025-06-30")

# -----------------------------------------------------------------------------
# 3. DOWNLOAD DAILY ADJUSTED CLOSE PRICES
# -----------------------------------------------------------------------------

get_daily_prices <- function(ticker, from, to){
  
  cat("Downloading:", ticker, "\n")
  
  data <- tryCatch({
    
    getSymbols(
      ticker,
      src = "yahoo",
      from = from,
      to = to,
      auto.assign = FALSE
    )
    
  }, error = function(e){
    
    cat("FAILED:", ticker, "\n")
    return(NULL)
    
  })
  
  if(is.null(data)){
    return(NULL)
  }
  
  # Adjusted Close
  price <- Ad(data)
  
  colnames(price) <- ticker
  
  return(price)
}


# -----------------------------------------------------------------------------
# 4. DOWNLOAD ALL STOCKS
# -----------------------------------------------------------------------------

download_group <- function(tickers, from, to){
  
  data_list <- lapply(
    tickers,
    get_daily_prices,
    from = from,
    to = to
  )
  
  # Remove failed downloads
  data_list <- data_list[!sapply(data_list, is.null)]
  
  # Combine all stocks
  prices <- Reduce(
    function(x, y) merge(x, y, join = "outer"),
    data_list
  )
  
  return(prices)
}


# Brown stock prices
brown_prices <- download_group(
  brown_tickers,
  start_date,
  end_date
)


# Green stock prices
green_prices <- download_group(
  green_tickers,
  start_date,
  end_date
)


# Check downloaded data
head(brown_prices)
head(green_prices)


# -----------------------------------------------------------------------------
# 5. DAILY LOG RETURN
# -----------------------------------------------------------------------------

# Formula:
#
# Daily log return =
# log(P_t) - log(P_(t-1))
#
# Equivalent to:
# log(P_t / P_(t-1))


daily_log_return <- function(price_data){
  
  ret <- diff(log(price_data))
  
  return(ret)
}


# Calculate DAILY log returns
brown_daily_logret <- daily_log_return(brown_prices)

green_daily_logret <- daily_log_return(green_prices)


# Check
head(brown_daily_logret)
head(green_daily_logret)


# -----------------------------------------------------------------------------
# 6. MONTHLY LOG RETURN
# -----------------------------------------------------------------------------

# IMPORTANT:
#
# We already calculated DAILY log returns.
#
# Monthly log return is obtained by:
#
# Monthly log return =
# SUM of daily log returns during that month
#
# Because:
#
# log(P_end/P_start)
# =
# sum[log(P_t/P_(t-1))]
#
#


monthly_log_return <- function(daily_returns){
  
  monthly_return <- apply.monthly(
    daily_returns,
    FUN = function(x){
      
      colSums(x, na.rm = TRUE)
      
    }
  )
  
  return(monthly_return)
}


# Calculate MONTHLY log returns
brown_monthly_logret <- monthly_log_return(
  brown_daily_logret
)

green_monthly_logret <- monthly_log_return(
  green_daily_logret
)


# Check monthly log returns
head(brown_monthly_logret)
head(green_monthly_logret)


# -----------------------------------------------------------------------------
# 7. SAVE MONTHLY LOG RETURNS
# -----------------------------------------------------------------------------

write.csv(
  data.frame(
    Date = index(brown_monthly_logret),
    coredata(brown_monthly_logret)
  ),
  "brown_monthly_log_returns.csv",
  row.names = FALSE
)


write.csv(
  data.frame(
    Date = index(green_monthly_logret),
    coredata(green_monthly_logret)
  ),
  "green_monthly_log_returns.csv",
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 8. PORTFOLIO WEIGHTS
# -----------------------------------------------------------------------------

# For now we use EQUAL WEIGHTS.
#
# Each stock receives:
#
# 1 / 15 = 0.06667
#
# If later you have market-cap data, we can change this to
# value-weighted returns.


brown_weights <- rep(
  1 / length(brown_tickers),
  length(brown_tickers)
)

names(brown_weights) <- brown_tickers


green_weights <- rep(
  1 / length(green_tickers),
  length(green_tickers)
)

names(green_weights) <- green_tickers


# Check weights
brown_weights
green_weights


# -----------------------------------------------------------------------------
# 9. WEIGHTED AVERAGE RETURN (WAR)
# -----------------------------------------------------------------------------

calculate_WAR <- function(monthly_returns, weights){
  
  # Keep only stocks available in the data
  common_stocks <- intersect(
    colnames(monthly_returns),
    names(weights)
  )
  
  monthly_returns <- monthly_returns[, common_stocks]
  
  weights <- weights[common_stocks]
  
  # Normalize weights
  weights <- weights / sum(weights)
  
  # Weighted return
  WAR <- xts(
    rowSums(
      sweep(
        coredata(monthly_returns),
        2,
        weights,
        "*"
      ),
      na.rm = TRUE
    ),
    order.by = index(monthly_returns)
  )
  
  colnames(WAR) <- "WAR"
  
  return(WAR)
}


# Brown WAR
WAR_brown <- calculate_WAR(
  brown_monthly_logret,
  brown_weights
)


# Green WAR
WAR_green <- calculate_WAR(
  green_monthly_logret,
  green_weights
)


# Check
head(WAR_brown)
head(WAR_green)


# -----------------------------------------------------------------------------
# 10. CARBON FACTOR (CF_t)
# -----------------------------------------------------------------------------

# Carbon Factor:
#
# CF_t = WAR_Brown,t - WAR_Green,t
#
# Positive CF_t:
# Brown portfolio performed better than Green portfolio.
#
# Negative CF_t:
# Green portfolio performed better than Brown portfolio.


CF <- merge(
  WAR_brown,
  WAR_green,
  join = "inner"
)


colnames(CF) <- c(
  "WAR_brown",
  "WAR_green"
)


CF$CF_t <- CF$WAR_brown - CF$WAR_green


# Check Carbon Factor
head(CF)

tail(CF)


# -----------------------------------------------------------------------------
# 11. SAVE CARBON FACTOR DATA
# -----------------------------------------------------------------------------

CF_data <- data.frame(
  Date = index(CF),
  coredata(CF)
)


write.csv(
  CF_data,
  "carbon_factor_CFt.csv",
  row.names = FALSE
)


# -----------------------------------------------------------------------------
# 12. STATIONARITY TEST
# -----------------------------------------------------------------------------

# We test STATIONARITY on RETURNS,
# NOT on raw stock price levels.
#
# ADF:
# H0 = unit root / non-stationary
#
# If p-value < 0.05:
# Reject H0 -> stationary
#
#
# KPSS:
# H0 = stationary
#
# If p-value > 0.05:
# Do not reject H0 -> stationary


stationarity_test <- function(series, name){
  
  series <- na.omit(as.numeric(series))
  
  # ADF test
  adf_result <- tryCatch(
    adf.test(series),
    error = function(e) NULL
  )
  
  # KPSS test
  kpss_result <- tryCatch(
    kpss.test(series, null = "Level"),
    error = function(e) NULL
  )
  
  
  data.frame(
    
    Series = name,
    
    ADF_Statistic =
      if(!is.null(adf_result))
        round(as.numeric(adf_result$statistic), 4)
    else NA,
    
    ADF_p_value =
      if(!is.null(adf_result))
        round(adf_result$p.value, 4)
    else NA,
    
    KPSS_Statistic =
      if(!is.null(kpss_result))
        round(as.numeric(kpss_result$statistic), 4)
    else NA,
    
    KPSS_p_value =
      if(!is.null(kpss_result))
        round(kpss_result$p.value, 4)
    else NA
    
  )
}


# -----------------------------------------------------------------------------
# 13. STATIONARITY OF CARBON FACTOR
# -----------------------------------------------------------------------------

CF_stationarity <- stationarity_test(
  CF$CF_t,
  "Carbon Factor CF_t"
)


print(CF_stationarity)


# -----------------------------------------------------------------------------
# 14. STATIONARITY OF BROWN AND GREEN WAR
# -----------------------------------------------------------------------------

brown_stationarity <- stationarity_test(
  WAR_brown$WAR,
  "Brown WAR"
)


green_stationarity <- stationarity_test(
  WAR_green$WAR,
  "Green WAR"
)


print(brown_stationarity)
print(green_stationarity)


# -----------------------------------------------------------------------------
# 15. COMBINE STATIONARITY RESULTS
# -----------------------------------------------------------------------------

stationarity_results <- rbind(
  brown_stationarity,
  green_stationarity,
  CF_stationarity
)


print(stationarity_results)


# Save results
write.csv(
  stationarity_results,
  "stationarity_results.csv",
  row.names = FALSE
)


print(stationarity_results)

