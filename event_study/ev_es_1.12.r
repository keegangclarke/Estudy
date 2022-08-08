# 1. User defined functions and package (library) imports #########################
print("Loading libraries and user-defined functions.")
start_time <- Sys.time()

library(estudy2)
library(magrittr)

fun_insert <- function(x, pos, insert) {
  # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"),
       paste0("\\1", insert, "\\2"),
       x)
}

fix_digit_names <- function(x, insertion, pos_idx = 0) {
  # NOTE: Need to assign it to the x variable
  # e.g. x <- tf_fixer(x, insertion)
  fun_insert <- function(x, pos, insert) {
    # Function inserts 'insertion' argument at the 0 position
    gsub(paste0("^(.{", pos, "})(.*)$"),
         paste0("\\1", insert, "\\2"),
         x)
  }
  
  test_vec <- grepl("^[[:digit:]+]", x)
  for (i in 1:length(x)) {
    if (test_vec[i] == TRUE) {
      x[i] <- fun_insert(x[i], pos_idx, as.character(insertion))
    }
  }
  return(x)
}

name_as_string <- function(x) {
  # Returns the name of whatever you pass as a character string
  # Primary purpose is to get string representation of function and variable names
  # Added benefit is anything passed as x will come out as 'x'
  deparse(substitute(x))
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")
# 2. Data Loading and basic wrangling #############################################
# Get directories of files
print("Fetching data...")
start_time <- Sys.time()

cd <-
  'C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/daily/cleaned/es_format/'
files <-
  c('stock_data_column-wise.csv',
    'market-index_data_column-wise.csv')

# Join the strings together (static = simpler in R, but wanted to practice looping over txt)
cds <- list()

for (i in files) {
  string <- paste(cd, i, sep = "")
  # print(string)
  cds <- append(cds, string, )
}

# Read in data
# No df1 because that would be the 3rd file containing everything in df2 and df3
df2 <- read.csv(as.character(cds[1]))
df3 <- read.csv(as.character(cds[2]))

# Remove duplicate index
df2 = subset(df2, select = -c(X))
df3 = subset(df3, select = -c(X))

# Rename 'Date' var into 'date'
names(df2)[names(df2) == 'Date'] <- 'date'
names(df3)[names(df3) == 'Date'] <- 'date'

end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")

print("Reformatting data.shape...")
start_time <- Sys.time()

# Reformatting 'date's into Date-class
df2$date <- as.Date(df2$date, format = "%Y-%m-%d")
df3$date <- as.Date(df3$date, format = "%Y-%m-%d")

# Index by dates
rownames(df2) <- as.Date(df2$date, format = "%Y-%m-%d")
rownames(df3) <- as.Date(df3$date, format = "%Y-%m-%d")

# Subset data.frames to the right size of useable data
df2 <- subset(df2, rownames(df2) > as.Date("2019-03-29"))
df3 <- subset(df3, rownames(df3) > as.Date("2019-03-29"))

df2 <- subset(df2, rownames(df2) < as.Date("2020-05-01"))
df3 <- subset(df3, rownames(df3) < as.Date("2020-05-01"))

# Store 'date' column
dates2 <- c(as.Date(df2$date))
dates3 <- c(as.Date(df3$date))

# Create new data.frames without 'date' cols since index by 'date'
df2 <- subset(df2, select = -date)
df3 <- subset(df3, select = -date)

# Remove remaining rows where all data points = NaN
df2 <- df2[rowSums(is.na(df2)) != ncol(df2),]
df3 <- df3[rowSums(is.na(df3)) != ncol(df3),]
# NOTE: Does not remove all remaining NaNs because of 'date' col

end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")

# 3. I.D. data wrangling (strings) ################################################
cat(
  "Fetching variable identification data.",
  "\n",
  "Creating variables. Reformatting for r syntax..."
)
start_time <- Sys.time()

cd_dict = "C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/index_member_dict_fixed.xlsx"
cd_problem_stocks = "C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/problem_id_s.txt"
name_dict = readxl::read_xlsx(cd_dict)
# read in fully formatted names of problematic stock series
problem_names = read.delim(cd_problem_stocks, header = FALSE)
problem_names <- unlist(problem_names, use.names = FALSE)

# Get colnames
keys <- colnames(name_dict)
# Replace spaces with '.' to be able to select columns from data.frame
keys <- as.vector(gsub(" ", ".", keys))

# Initialise empty lists with required length
# indice_list <- vector(mode = "list", length = n)
stock_list <- vector(mode = "list", length = length(df3))
market_list <- vector(mode = "list", length = length(df3))

# Swap numerical index for named index --> provides similarity to a python dictionary
names(market_list) <- keys
names(stock_list) <- keys

# Specify patterns
pattern_list <- c(" ", "/", "-", "\\*", "&")

# Break ID data.frame into list to remove NANs
name_list <- as.list(name_dict)
for (i in 1:3) {
  name_list <-
    lapply(
      name_list,
      stringr::str_replace_all,
      pattern = paste0(pattern_list, collapse = "|"),
      replacement = "."
    )
  name_list <- lapply(name_list, na.omit)
  name_list <- lapply(name_list, fix_digit_names, "X")

  # Make syntactically compatible
  names(name_list) <-
    lapply(names(name_list),
           gsub,
           pattern = " ",
           replacement = ".")

}

end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")

# 4. Creation of list of market-index data.frames ##############################

print("Making list of market data.frame")
start_time <- Sys.time()

for (i in 1:length(keys)) {
  # Select as.data.frame the market index
  temp_df <- as.data.frame(df3[, keys[[i]], drop = FALSE])
  temp_df <- cbind(date = dates2, temp_df)
  
  market_list[[keys[[i]]]] <- temp_df
  
  rm(temp_df)
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")
# 5. data.frame2 Slicing process###################################################
print("Begining 'data.frame 2' (df2) slicing process.")
start_time <- Sys.time()

for (i in 1:length(keys)) {
  tryCatch({
    print(i)
    print(keys[[i]])
    # gets names corresponding to key: gets constituents of index
    str_vec <- unlist(name_list[[keys[[i]]]], use.names = FALSE)
    
    # selects index by their names
    stock_series <- df2[str_vec]
    stock_series <- cbind(date = dates2, stock_series)
    # stock_series <- subset(df2, select=str_vec) #df2 = subset(df2, select = -c(X))
    
    # remove NA rows from data.frame
    stock_series <-
      stock_series[rowSums(is.na(stock_series)) != ncol(stock_series),]
    
    # stores data in dictionary of index constituent pd.DataFrame --> Index name is key
    stock_list[[keys[[i]]]] <- stock_series
  }, error = function(e) {
    message(cat("ERROR: ", conditionMessage(e), "\n"))
  })
}
# NOTE: THERE ARE STILL NANs PRESENT IN THE ORGANISED DATASET
end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")

# 6. Recording of problem data for inspection. STATUS = INACTIVE ########
# write problem data to csv to view in excel
# problem_list <-
#   c(
#     "KOSPI.Index",
#     "JCI.Index",
#     "SPX.Index",
#     "SHSZ300.Index",
#     "SPTSX.Index",
#     "SASEIDX.Index",
#     "AS51.Index",
#     "JALSH.Index",
#     "XU100.Index",
#     "N100.Index",
#     "HSI.Index"
#   )
# write filenames
# cd_prob <- "C:/Users/Keegan/Desktop/staging/problem_stocks/"
# problem_filenames <- vector(mode = "character",length=length(problem_list))
# for (i in 1:length(problem_list)) {
#   fname <- paste0(problem_list[[i]],".csv")
#   directory <- paste0(cd_prob,fname)
#   problem_filenames[[i]] = directory
#   rm(fname)
#   rm(directory)
# }

# Writes problem data in specified place in order to visually inspect
# problem_stocks <- stock_list[problem_list]
# for (i in 1:length(problem_list)) {
#   temp_selector <- problem_stocks[[i]]
#   write.csv(temp_selector,
#             file = problem_filenames[[i]]
#             )
# }

# 7. Removal of remaining NAs ##################################################
cat(
  "Removing NANs for data.frames in lists:",
  "\n",
  "1. ",
  name_as_string(stock_list),
  "\n",
  "2. ",
  name_as_string(market_list),
  "\n"
)
start_time <- Sys.time()

# Copy lists of data.frames for verification purposes
market_list_copy <- market_list
stock_list_copy <- stock_list

# 'NA' CLEANING LOOPS
# for the MARKET-data
for (i in 1:length(market_list)) {
  # selects data.frame in list that is to be manipulated on this iter
  # INPUT
  selector <- market_list[[i]]
  # Make copy of data column in order to handle N*2 data.frames
  selector$copy = selector[[ncol(selector)]]
  # drop the 'date' column
  selector <- subset(selector, select = -c(date))
  # remove rows where all observations = 'NA'
  selector <- selector[rowSums(is.na(selector)) != ncol(selector), ]
  # remove columns where at least 1 observation = 'NA'
  selector <- selector[, colSums(is.na(selector)) == 0]
  # add back the date column
  selector <- tibble::rownames_to_column(selector)
  selector <- dplyr::rename(selector, date = rowname)
  rownames(selector) <- selector$date
  # remove the 'copy' column as it is no longer needed
  selector <- subset(selector, select = -c(copy))
  # return cleaned data.frame to list of data.frames
  # OUTPUT
  market_list[[i]] <- selector
  rm(selector)
}
# for the STOCKS (market-constituient) data
for (i in 1:length(stock_list)) {
  # selects data.frame in list that is to be manipulated on this iter
  # INPUT
  selector <- stock_list[[i]]
  # drop the 'date' column
  selector <- subset(selector, select = -c(date))
  # remove rows where all observations = 'NA'
  selector <- selector[rowSums(is.na(selector)) != ncol(selector), ]
  # remove columns where at least 1 observation = 'NA'
  selector <- selector[, colSums(is.na(selector)) == 0]
  # add back the date column
  selector <- tibble::rownames_to_column(selector)
  selector <- dplyr::rename(selector, date = rowname)
  rownames(selector) <- selector$date
  # return cleaned data.frame to list of data.frames
  # OUTPUT
  stock_list[[i]] <- selector
  rm(selector)
}

# CHECK CLEANING HAPPENED
if ((identical(market_list, market_list_copy) == TRUE) &
    (identical(stock_list, stock_list_copy) == TRUE)) {
  message(
    cat(
      "WARNING: Attempted NAN removal has resulted in identical lists.",
      "\n",
      "1.",
      name_as_string(market_list)
    ),
    "\n",
    "2. ",
    name_as_string(stock_list)
  )
} else if (identical(market_list, market_list_copy) == TRUE) {
  message(
    cat(
      "WARNING: Attempted NAN removal has resulted in identical lists.",
      "\n",
      "1.",
      name_as_string(market_list)
    )
  )
} else if (identical(stock_list, stock_list_copy) == TRUE) {
  message(
    cat(
      "WARNING: Attempted NAN removal has resulted in identical lists.",
      "\n",
      "1.",
      name_as_string(stock_list)
    )
  )
} else {
  message(
    cat(
      "NANs have been removed from the following lists",
      "\n",
      "1. ",
      name_as_string(market_list),
      "\n",
      "2. ",
      name_as_string(stock_list)
    )
  )
}
end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")

# 8. Application of Estudy2 ########################################################
start_time <- Sys.time()
# Create data storage lists
reg_results_list <-
  vector(mode = "list", length = length(market_list))
names(reg_results_list) <- keys
# abnormal return
ar_test_results_list <-
  vector(mode = "list", length = length(market_list))
names(ar_test_results_list) <- keys
# cumulative abnormal return
car_test_results_list <-
  vector(mode = "list", length = length(market_list))
names(car_test_results_list) <- keys

# Large loop that applies Estudy process over the large dataset for GEOGRAPHIC regions
for (i in 1:length(market_list)) {
  tryCatch({
    print(paste("Getting rates from prices for", keys[[i]]))
    rates <- get_rates_from_prices(
      stock_list[[i]],
      quote = "Close",
      multi_day = TRUE,
      compounding = "continuous"
    )
    
    rates_indx <- get_rates_from_prices(
      market_list[[i]],
      quote = "Close",
      multi_day = TRUE,
      compounding = "continuous"
    )
    
    # FIX DTYPES OF COLUMN PRIOR TO TESTING
    rates <- transform.data.frame(rates, date = as.Date(date))
    rates_indx <-
      transform.data.frame(rates_indx, date = as.Date(date))
    print("Done. Applying single-index market model.")
    
    # apply single-index market model to get ARs
    securities_returns <- apply_market_model(
      rates = rates,
      regressor = rates_indx,
      same_regressor_for_all = TRUE,
      market_model = "sim",
      estimation_method = "ols",
      estimation_start = as.Date("2019-04-01"),
      estimation_end = as.Date("2020-03-13")
    )
    
    print("Done. Applying parametric and non-parametric tests to abnormal returns...")
    # ABNORMAL RETURN TESTS
    # Parametric tests
    ar_para <- data.frame(
      parametric_tests(
        list_of_returns = securities_returns,
        event_start = as.Date("2020-03-16"),
        event_end = as.Date("2020-03-20")
      )
    )
    # Non-parametric tests
    ar_non_para <- data.frame(
      nonparametric_tests(
        list_of_returns = securities_returns,
        event_start = as.Date("2020-03-16"),
        event_end = as.Date("2020-03-20")
      )
    )
    # CUMULATIVE ABNORMAL RETURN TESTS
    # Parametric tests
    car_para <- data.frame(
      car_parametric_tests(
        list_of_returns = securities_returns,
        car_start = as.Date("2020-03-16"),
        car_end = as.Date("2020-03-20")
      )
    )
    # Non-parametric tests
    car_non_para <- data.frame(
      car_nonparametric_tests(
        list_of_returns = securities_returns,
        car_start = as.Date("2020-03-16"),
        car_end = as.Date("2020-03-20")
      )
    )
    print(paste("Merging results.", keys[[i]]))
    # Stage results for storage
    ar_results <- data.frame(merge(ar_para, ar_non_para, by = "date"))
    car_results <- dplyr::bind_rows(car_para, car_non_para)
    # Clean up 'ar_results'
    ar_results <- subset(ar_results, select = -c(weekday.y))
    ar_results <-
      dplyr::rename(
        ar_results,
        weekday = weekday.x,
        pct.para = percentage.x,
        pct.nonpara = percentage.y
      )
    # Store results for later recording
    ar_test_results_list[[keys[[i]]]] <- ar_results
    car_test_results_list[[keys[[i]]]] <- car_results
    reg_results_list[[keys[[i]]]] <- securities_returns
    # Explicit memory cleanup
    rm(ar_results)
    rm(car_results)
  }, error = function(e) {
    message(cat("ERROR: ", conditionMessage(e), "i = ", i , "\n"))
  })
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")

# 9. Recording of results in new datafiles #####################################
start_time <- Sys.time()

# WRITE RESULTS
cd_ar <- "C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/geographic_region/abnormal_returns/"
cd_car <- "C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/geographic_region/cumulative_abnormal_returns/"
ar_results_filenames <-
  vector(mode = "character", length = length(ar_test_results_list))
car_results_filenames <-
  vector(mode = "character", length = length(car_test_results_list))
# Create file-directory strings
# AR DIRECTORIES
for (i in 1:length(ar_test_results_list)) {
  directory <- paste0(cd_ar, paste0(keys[[i]], ".csv"))
  ar_results_filenames[[i]] = directory
  rm(directory)
}
# CAR DIRECTORIES
for (i in 1:length(car_test_results_list)) {
  directory <- paste0(cd_car, paste0(keys[[i]], ".csv"))
  car_results_filenames[[i]] = directory
  rm(directory)
}
# Writes results data in specified place
# AR RESULTS
for (i in 1:length(ar_test_results_list)) {
  write.csv(ar_test_results_list[[i]],
            file = ar_results_filenames[[i]])
}
# CAR RESULTS
for (i in 1:length(car_test_results_list)) {
  write.csv(car_test_results_list[[i]],
            file = car_results_filenames[[i]])
}
end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")
print("Terminating script...")