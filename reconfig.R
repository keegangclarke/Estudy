# recombine, reconfigure & reconstruct func
# OBJECTIVE:
# INPUT: 'list of lists of lists' object, levels are descending in hierarchy
# FUNC:   
      # recombine into 'list of lists', matching item for item on the 3rd level of the object, class('returns')
      # reconfigure so that constituents are of the smallest length, & params are set to the smallest one
      # re-list the 3rd level into 'returns' objects
# OUTPUT: 'list of lists of lists' object
require(zoo)
require(magrittr)
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

# ASSITANT FUNCTIONS
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

make_list <- function(Length, Names) {
  # Reduces code lines later
  vec <- vector(mode = 'list', length = Length)
  names(vec) <- Names
  return(vec)
}

# PRIMARY FUNCTION
reconfig <- function(lolol, dropNA = TRUE) {
  # SET KEY VARIABLES
  tickers <- names(lolol)
  returns_names <- names(lolol[[1]])
  LENGTH <- length(lolol)
  
  # PREPARATION FOR EXTRACTION PROCESS ####
  # VARIABLES
  # ZOOs
  observed <- make_list(LENGTH, tickers)
  predicted <- make_list(LENGTH, tickers)
  lower95CI <- make_list(LENGTH, tickers)
  upper95CI <- make_list(LENGTH, tickers)
  abnormal <- make_list(LENGTH, tickers)
  regressor <- make_list(LENGTH, tickers)
  # STRINGs
  market_model <- make_list(LENGTH, tickers)
  full_name_market_model <- make_list(LENGTH, tickers)
  estimation_method <- make_list(LENGTH, tickers)
  full_name_estimation_method <- make_list(LENGTH, tickers)
  # DATEs
  estimation_start <- make_list(LENGTH, tickers)
  estimation_end <- make_list(LENGTH, tickers)
  # MISC
  estimation_length <- make_list(LENGTH, tickers)
  coefficients <- make_list(LENGTH, tickers)
  
  # EXTRACTION PROCESS ####
  for (i in seq_along(lolol)) {
    # 1. SELECT ITEMS
    rets <- lolol[[i]]
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
    estimation_start[[i]] <- rets$estimation_start
    estimation_end[[i]] <- rets$estimation_end
    # MISC
    # list of numeric vectors
    coefficients[[i]] <- rets$coefficients
    # Numeric vector
    estimation_length[[i]] <- rets$estimation_length
    
    # CLEAN-UP
    rm(rets)
  }
  # COMBINE ZOOs
  # aggregate as ZOOs
  observed <- merge_zoo_list(observed)
  predicted <- merge_zoo_list(predicted)
  lower95CI <- merge_zoo_list(lower95CI)
  upper95CI <- merge_zoo_list(upper95CI)
  abnormal <- merge_zoo_list(abnormal)
  regressor <- merge_zoo_list(regressor)
  
  # UNLIST STORAGE LISTS
  estimation_length <- estimation_length %>% unlist
  # SPECIFY FINAL VARIABLE
  estimation_start <- estimation_start %>% unlist %>% as.Date
  if (all(estimation_start %in% estimation_start[[1]])) {
    estimation_start <- estimation_start[[1]]
  } else {
    estimation_start <-  max(estimation_start)
  }
  estimation_end <- estimation_end %>% unlist %>% as.Date
  if (all(estimation_end %in% estimation_end[[1]])) {
    estimation_end <- estimation_end[[1]]
  } else {
    estimation_end <-  min(estimation_end)
  }
  
  if (dropNA == TRUE) {
    observed <- na.omit(observed)
    predicted <- na.omit(predicted)
    lower95CI <- na.omit(lower95CI)
    upper95CI <- na.omit(upper95CI)
    abnormal <- na.omit(abnormal)
    regressor <- na.omit(regressor)
  }
  
  
  # RECOMBINATION PROCESS ####
  # create identical storage structure
  # empty so that it throws errors if recombination fails
  lolol.out <- vector(mode = 'list',
                      length = length(lolol)) %>%
    set_names(tickers)
  
  # RECOMBINE
  for (i in seq_along(lolol)) {
    # 1. SELECT ITEMS
    tick <- tickers[[i]]
    rets <- vector(mode = 'list',
                   length = length(lolol[[tick]])) %>%
      set_names(names(lolol[[tick]])) %>%
      set_class('returns')
    
    # ZOOs
    # aggregate as ZOOs
    rets$observed <- observed[, i]
    rets$predicted <- predicted[, i]
    rets$lower95CI <- lower95CI[, i]
    rets$upper95CI <- upper95CI[, i]
    rets$abnormal <- abnormal[, i]
    rets$regressor <- regressor[, i]
    # STRINGs
    # aggregate as lists
    rets$market_model <- market_model
    rets$full_name_market_model <- full_name_market_model
    rets$estimation_method <- estimation_method
    rets$full_name_estimation_method <- full_name_estimation_method
    # DATEs
    # aggregate as lists
    rets$estimation_start <- estimation_start
    rets$estimation_end <- estimation_end
    # MISC
    # list of numeric vectors
    rets$coefficients <- coefficients[[tick]]
    # Numeric vector
    rets$estimation_length <- estimation_length[[tick]]
    
    # 3. RECOMBINE
    lolol.out[[tick]] <- rets
    
    # CLEAN-UP
    rm(rets)
  }
  return (lolol.out)
}