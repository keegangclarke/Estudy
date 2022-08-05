# 1. User defined functions and package (library) imports #########################
print("Loading libraries and user-defined functions.")
start_time <- Sys.time()

library(estudy2)

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
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")
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
  cds <- append(cds, string,)
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
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

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
df2 <- df2[rowSums(is.na(df2)) != ncol(df2), ]
df3 <- df3[rowSums(is.na(df3)) != ncol(df3), ]
# NOTE: Does not remove all remaining NaNs because of 'date' col

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

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

# copy df3 (market data) into list of market's and their data
# NOTE: SEEMS TO BE REDUNDANT COPYING
df_3 <- as.list(df3)

# Get colnames
keys <- colnames(name_dict)

# Replace spaces with '.' to be able to select columns from data.frame
keys2 <- as.vector(gsub(" ", ".", keys))

# Initialise empty lists with required length
# indice_list <- vector(mode = "list", length = n)
stock_list <- vector(mode = "list", length = length(df_3))
market_list <- vector(mode = "list", length = length(df_3))

# Swap numerical index for named index --> provides similarity to a python dictionary
names(market_list) <- keys2
names(stock_list) <- keys2

# Specify patterns 
pattern_list <- c(" ", "/", "-", "\\*", "&")
trouble <-
  c(
    "X009900.KS.Equity",
    "ADMR.IJ.Equity",
    "AEP.UW.Equity",
    "X300866.CH.Equity",
    "CTS.CT.Equity",
    "ACWA.AB.Equity",
    "DRR.AT.Equity",
    "BHG.SJ.Equity",
    "AYDEM.TI.Equity",
    "STLA.FP.Equity",
    "X9618.HK.Equity",
    "IVG.IM.Equity",
    "STLA.FP.Equity", #NOTE: this name is a double-up, but does not break code
    "ENR.GY.Equity",
    "HON.UW.Equity",
    "X360.AT.Equity"
  )
trouble_selector <- unique(trouble)
trouble_selector <- paste("^",trouble_selector,"$", sep="")
trouble_selector <- paste0(trouble_selector, collapse = "|")

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
  name_list <- lapply(
    name_list,
    stringr::str_replace_all,
    pattern = trouble_selector,
    replacement = NA_character_
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
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

# 4. Removal of empty columns #######################################################
print("Dropping problematic share names.")
start_time <- Sys.time()

# Allocate colnames
names_to_keep <- names(df2)[!(names(df2) %in% trouble)]
names_to_keep2 <- names(df2)[!(names(df2) %in% problem_names)]

# Drop from data
df2_trouble <- subset(df2, select = trouble)
df2 <- subset(df2, select = names_to_keep)

df2_problems <- subset(df2, select = problem_names)
# df2 <- subset(df2, select = names_to_keep2)
df2_test <- df2[,!(names(df2) %in% problem_names)]

# Check carve happened correctly
if ((any(trouble %in% names(df2)))==TRUE || (any(problem_names %in% names(df2)))==TRUE) {
  message("WARNING: Removal of problem columns failed")
} 

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

# 5. Creation of list of market-index data.frames ##############################

print("Making list of market data.frame")
start_time <- Sys.time()

for (i in 1:length(keys2)) {
  # Select as.data.frame the market index
  temp_df <- as.data.frame(df3[, keys2[[i]], drop = FALSE])
  temp_df <- cbind(date = dates2, temp_df)
  
  market_list[[keys2[[i]]]] <- temp_df  

  rm(temp_df)
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")
# 6. data.frame2 Slicing process###################################################
print("Begining 'data.frame 2' (df2) slicing process.")
start_time <- Sys.time()

for (i in 1:length(keys2)) {
  tryCatch({
    print(i)
    print(keys2[[i]])
    # gets names corresponding to key: gets constituents of index
    str_vec <- unlist(name_list[[keys2[[i]]]], use.names = FALSE)
    
    # selects index by their names
    stock_series <- df2[str_vec]
    stock_series <- cbind(date = dates2, stock_series)
    # stock_series <- subset(df2, select=str_vec) #df2 = subset(df2, select = -c(X))
    
    # remove NA rows from data.frame
    stock_series <- stock_series[rowSums(is.na(stock_series)) != ncol(stock_series), ]
    
    # stores data in dictionary of index constituent pd.DataFrame --> Index name is key
    stock_list[[keys2[[i]]]] <- stock_series
  }, error = function(e) {
    message(cat("ERROR: ", conditionMessage(e), "\n"))
  })
}
# NOTE: THERE ARE STILL NANs PRESENT IN THE ORGANISED DATASET
end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

# 6.b Recording of problem data for inspection. Status = Inactive ########
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
  "Removing NANs for data.frames in lists:", "\n",
  "1. ", name_as_string(stock_list),"\n",
  "2. ", name_as_string(market_list),"\n"
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
  selector <- selector[rowSums(is.na(selector)) != ncol(selector),]
  # remove columns where at least 1 observation = 'NA'
  selector <- selector[, colSums(is.na(selector)) == 0]
  # add back the date column
  selector <- tibble::rownames_to_column(selector)
  selector <- dplyr::rename(selector,date=rowname)
  rownames(selector) <- selector$date
  # remove the 'copy' column as it is no longer needed
  selector <- subset(selector, select = -c(copy))
  # change dtype of 'date' from 'character' dtype to 'Date' dtype
  # selector$date <- as.Date(as.character(selector$date))
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
  selector <- selector[rowSums(is.na(selector)) != ncol(selector),]
  # remove columns where at least 1 observation = 'NA'
  selector <- selector[, colSums(is.na(selector)) == 0]
  # add back the date column
  selector <- tibble::rownames_to_column(selector)
  selector <- dplyr::rename(selector,date=rowname)
  rownames(selector) <- selector$date
  # change dtype of 'date' from 'character' dtype to 'Date' dtype
  # selector$date <- as.Date(as.character(selector$date))
  # return cleaned data.frame to list of data.frames
  # OUTPUT
  stock_list[[i]] <- selector
  rm(selector)
}

# CHECK CLEANING HAPPENED
if ((identical(market_list,market_list_copy) == TRUE)&(identical(stock_list,stock_list_copy) == TRUE)) {
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n",
              "1.", name_as_string(market_list)), "\n",
              "2. ", name_as_string(stock_list)
          )
} else if (identical(market_list,market_list_copy) == TRUE) {
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n",
              "1.", name_as_string(market_list))
          )
} else if (identical(stock_list,stock_list_copy) == TRUE) {
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n",
              "1.", name_as_string(stock_list)))
} else {
  message(cat("NANs have been removed from the following lists", "\n",
              "1. ", name_as_string(market_list), "\n",
              "2. ", name_as_string(stock_list))
          )
}
end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

# 8. Application of Estudy2 ########################################################
start_time <- Sys.time()
# Create data storage lists
reg_results_list <-
  vector(mode = "list", length = length(market_list))
test_results_list <-
  vector(mode = "list", length = length(market_list))

# Large loop that applies Estudy process over the large dataset for GEOGRAPHIC regions
for (i in 1:length(market_list)) {
  tryCatch({
    # print("Getting rates from prices...")
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
    rates <- transform.data.frame(rates,date=as.Date(date))
    rates_indx <- transform.data.frame(rates_indx,date=as.Date(date))
    
    # print("Removing NANs from rates...")
    # rates <- na.omit(rates)
    # rates_indx <- na.omit(rates_index)
    # print("Done. Applying single-index market model...")
    
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
    # print("Done. Applying parametric and non-parametric tests to abnormal returns...")

    # TESTS
    # Parametric tests
    para <- data.frame(
      parametric_tests(
        list_of_returns = securities_returns,
        event_start = as.Date("2020-03-16"),
        event_end = as.Date("2020-03-20")
      )
    )
    # Non-parametric tests
    non_para <- data.frame(
      nonparametric_tests(
        list_of_returns = securities_returns,
        event_start = as.Date("2020-03-16"),
        event_end = as.Date("2020-03-20")
      )
    )
    # print("Merging results.")
    # all_tests <- merge(para, non_para, by = "date")
    df_results <- data.frame(merge(para, non_para, by = "date"))
    
    test_results_list[[keys2[[i]]]] <- df_results
    reg_results_list[[keys2[[i]]]] <- securities_returns
    
  }, error = function(e) {
    message(cat("ERROR: ", conditionMessage(e), "i = ", i , "\n"))
  })
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")

# 9. Recording of results in new datafiles #####################################
start_time <- Sys.time()
# Specify directories

# Write results
# write.csv()
# write.csv()

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time-start_time,digits = 4), "seconds")
print("Terminating script...")