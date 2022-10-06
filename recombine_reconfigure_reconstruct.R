# recombine, reconfigure & reconstruct func
# OBJECTIVE:
# INPUT: 'list of lists of lists' object, levels are decending in hieracrchy
# FUNC:   
      # recombine into 'list of lists', matching item for item on the 3rd level of the object, class('returns')
      # reconfigure so that constituents are of the smallest length, & params are set to the smallest one
      # re-list the 3rd level into 'returns' objects
# OUTPUT: 'list of lists of lists' object
library(zoo)
library(magrittr)
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

lolol <- securities_returns

tickers <- names(securities_returns)
returns_names <- names(securities_returns[[1]])

samp1 <- lolol$X000020.KS.Equity
samp2 <- lolol$X4523.JT.Equity

observed1 <- samp1$observed
observed2 <- samp2$observed

synth <- list(observed1,observed2,observed1,observed2) %>% set_names(c("1","2","three","4"))

merge_zoo_list <- function(zoo_list) {
  # Record names
  if (is.null(names(zoo_list))) {
    warning("List of Zoo objects is not named. Object returned will have no column names.")
  }
  all_names <- names(zoo_list)
  
  # Split data
  zoo_obj <- zoo_list[[1]]
  rest <- zoo_list[-1]
  
  # Merge into single ZOO
  for (i in seq_along(rest)) {
    zoo_obj <- zoo::merge.zoo(zoo_obj, rest[[i]])
  }
  
  # Reset names
  colnames(zoo_obj) <- all_names
  return(zoo_obj)
}

# PREPARATION FOR REALLOCATION PROCESS ####
# VARIABLES
# ZOOs
observed <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
predicted <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
lower95CI <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
upper95CI <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
abnormal <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
regressor <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
# STRINGs
market_model <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
full_name_market_model <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
estimation_method <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
full_name_estimation_method <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
# DATEs
estimation_start <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
estimation_end <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
# MISC 
estimation_length <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)
coefficients <- vector(mode = 'list', length = length(lolol)) %>% set_names(tickers)

# EXTRACTION PROCESS ####
for (i in seq_along(lolol)) {
  # 1. SELECT ITEMS
  rets <- lolol.c[[i]]
  # ZOOs
  # aggregate as ZOOs
  observed[[i]] <- rets$observed
  predicted[[i]] <- rets$predicted
  lower95CI[[i]] <- rets$lower95CI
  upper95CI[[i]] <- rets$upper95CI
  abnormal[[i]] <- rets$abnormal
  regressor[[i]] <- rets$regressor
  # STRINGs
  # aggregate as lists
  market_model <- rets$market_model
  full_name_market_model <- rets$full_name_market_model
  estimation_method <- rets$estimation_method
  full_name_estimation_method <- rets$full_name_estimation_method
  # DATEs
  # aggregate as lists
  estimation_start <- rets$estimation_start
  estimation_end <- rets$estimation_end
  # MISC
  # list of numeric vectors
  coefficients <- rets$coefficients
  # Numeric vector
  estimation_length <- rets$estimation_length
}

# COMBINE ZOOs
# aggregate as ZOOs
observed <- merge_zoo_list(observed)
predicted <- merge_zoo_list(predicted)
lower95CI <- merge_zoo_list(lower95CI)
upper95CI <- merge_zoo_list(upper95CI)
abnormal <- merge_zoo_list(abnormal)
regressor <- merge_zoo_list(regressor)
