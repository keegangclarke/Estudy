# Script will perform Sectorised Case-study 
# DATA WRANGLING: CLEANING #### 
# 1. User defined functions and package (library) imports ####
print("Loading libraries and user-defined functions.")
start_time <- Sys.time()

library(estudy2)
library(magrittr)

fun_insert <- function(x, pos, insert)
{
  # Create own function
  gsub(paste0("^(.{", pos, "})(.*)$"), paste0("\\1", insert, "\\2"), x)
}

fix_digit_names <- function(x, insertion, pos_idx = 0)
{
  # NOTE: Need to assign it to the x variable e.g. x <- tf_fixer(x, insertion)
  fun_insert <- function(x, pos, insert)
  {
    # Function inserts 'insertion' argument at the 0 position
    gsub(paste0("^(.{", pos, "})(.*)$"), paste0("\\1", insert, "\\2"), x)
  }
  
  test_vec <- grepl("^[[:digit:]+]", x)
  for (i in 1:length(x))
  {
    if (test_vec[i] == TRUE)
    {
      x[i] <- fun_insert(x[i], pos_idx, as.character(insertion))
    }
  }
  return(x)
}

name_as_string <- function(x)
{
  # Returns the name of whatever you pass as a character string Primary purpose is to get string
  # representation of function and variable names Added benefit is anything passed as x will come
  # out as 'x'
  deparse(substitute(x))
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")
# 2. Data Loading ##### 
# Get directories of files
print("Fetching data...")
start_time <- Sys.time()

cd <- "C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/daily/cleaned/es_format/"
files <- c("stock_data_column-wise.csv", "market-index_data_column-wise.csv")

# Join the strings together (static = simpler in R, but wanted to practice looping over txt)
cds <- list()

for (i in files)
{
  string <- paste(cd, i, sep = "")
  # print(string)
  cds <- append(cds, string, )
}

# Read in data No df1 because that would be the 3rd file containing everything in df2 and df3
df2 <- read.csv(as.character(cds[1]))
df3 <- read.csv(as.character(cds[2]))

# Remove duplicate index
df2 = subset(df2, select = -c(X))
df3 = subset(df3, select = -c(X))

# Rename 'Date' var into 'date'
names(df2)[names(df2) == "Date"] <- "date"
names(df3)[names(df3) == "Date"] <- "date"

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# 3. Basic wrangling of data (slicing data-set out of bulk, reformatting of dtypes) ####

print("Reformatting data.shape...")
start_time <- Sys.time()

# Reformatting 'date's into Date-class
df2$date <- as.Date(df2$date, format = "%Y-%m-%d")
df3$date <- as.Date(df3$date, format = "%Y-%m-%d")

# Index by dates
rownames(df2) <- as.Date(df2$date, format = "%Y-%m-%d")
rownames(df3) <- as.Date(df3$date, format = "%Y-%m-%d")

# Subset data.frames to the right size of usable data
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
# NOTE: Does not remove all remaining NaNs because of 'date' col have not tested if the above two
# lines can be removed without effects

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# 4. Wrangling of Identification (strings) data ##################
cat("Fetching variable identification data.", "\n", "Creating variables. Reformatting for r syntax...")
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

# Initialise empty lists with required length indice_list <- vector(mode = 'list', length = n)
stock_list <- vector(mode = "list", length = length(df3))
market_list <- vector(mode = "list", length = length(df3))

# Swap numerical index for named index --> provides similarity to a python dictionary
names(market_list) <- keys
names(stock_list) <- keys

# Specify patterns
pattern_list <- c(" ", "/", "-", "\\*", "&", ",")

# Break ID data.frame into list to remove NANs
name_list <- as.list(name_dict)
for (i in 1:3)
{
  name_list <- lapply(name_list, stringr::str_replace_all, pattern = paste0(pattern_list, collapse = "|"),
                      replacement = ".")
  name_list <- lapply(name_list, na.omit)
  name_list <- lapply(name_list, fix_digit_names, "X")
  
  # Make syntactically compatible
  names(name_list) <- lapply(names(name_list), gsub, pattern = " ", replacement = ".")
}
end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# 5. Creation of list of market-index data.frames ##############################

print("Making list of market data.frame")
start_time <- Sys.time()

for (i in 1:length(keys))
{
  # Select as.data.frame the market index
  temp_df <- as.data.frame(df3[, keys[[i]], drop = FALSE])
  temp_df <- cbind(date = dates2, temp_df)
  
  market_list[[keys[[i]]]] <- temp_df
  
  rm(temp_df)
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")
# 6. Creation of list of stock-index data.frames consisting of constituent-shares ####
print("Begining 'data.frame 2' (df2) slicing process.")
start_time <- Sys.time()

for (i in 1:length(keys))
{
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
    stock_series <- stock_series[rowSums(is.na(stock_series)) != ncol(stock_series), ]
    
    # stores data in dictionary of index constituent pd.DataFrame --> Index name is key
    stock_list[[keys[[i]]]] <- stock_series
    
    # explicit memory clean-up
    rm(stock_series)
  }, error = function(e)
  {
    message(cat("ERROR: ", conditionMessage(e), "\n"))
  })
}
# NOTE: THERE ARE STILL NANs PRESENT IN THE ORGANISED DATASET
end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# 7. Removal of remaining NAs ##### 
# Process first drops rows where all observations are NAs (public holidays, etc.)  
# Process then drops remaining columns that still include AT LEAST 1 NA
cat("Removing NANs for data.frames in lists:", "\n", "1. ", name_as_string(stock_list), "\n", "2. ",
    name_as_string(market_list), "\n")
start_time <- Sys.time()

# Copy lists of data.frames for verification purposes
market_list_copy <- market_list
stock_list_copy <- stock_list

# 'NA' CLEANING LOOPS for the MARKET-data
for (i in 1:length(market_list))
{
  # selects data.frame in list that is to be manipulated on this iter INPUT
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
  # return cleaned data.frame to list of data.frames OUTPUT
  market_list[[i]] <- selector
  rm(selector)
}
# for the STOCKS (market-constituient) data
for (i in 1:length(stock_list))
{
  # selects data.frame in list that is to be manipulated on this iter INPUT
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
  # return cleaned data.frame to list of data.frames OUTPUT
  stock_list[[i]] <- selector
  rm(selector)
}

# CHECK CLEANING HAPPENED
if ((identical(market_list, market_list_copy) == TRUE) & (identical(stock_list, stock_list_copy) == TRUE))
{
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n", "1.", name_as_string(market_list)),
          "\n", "2. ", name_as_string(stock_list))
} else if (identical(market_list, market_list_copy) == TRUE)
{
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n", "1.", name_as_string(market_list)))
} else if (identical(stock_list, stock_list_copy) == TRUE)
{
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n", "1.", name_as_string(stock_list)))
} else
{
  message(cat("NANs have been removed from the following lists", "\n", "1. ", name_as_string(market_list),
              "\n", "2. ", name_as_string(stock_list)))
}
end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")


# DATA WRANGLING: REARRANGEMENT ####

# OBJECTIVE: take data that I have and re-organise into the correct format 
# COMPLICATION: a. For each share, their relevant market-proxy is their index.
# Therefore, in order to accomplish a sectorised estudy I need to fist estimate
# the securities returns before reorganising.
# PROPOSED SOLUTION: 
# a. Run the loop to calc the rates and security return (ols) models, 
# Reorganise the models according to sectorisation
# feed reorganised models to remainder of estimation loop
# record results of tests in '.csv' files 

# 0. Cleanup of memory by removing old objects ####
rm(cds, df2, df3, market_list_copy, stock_list_copy, name_dict)

# 1. Obtain the right names
# Record names of remaining shares
remainder <- vector(mode="list", length=length(market_list))
names(remainder) <- keys

for (i in 1:length(remainder)) 
{
  # Select stock series' names and remove 'date'
  selector <- names(stock_list[[keys[[i]]]])
  selector <- selector[! selector %in% 'date']
  # store remainder in 'remainder' 
  remainder[[keys[[i]]]] <- selector
  rm(selector)
}

# 2. OLS model creation loop that statistics will be run on. ####
start_time <- Sys.time()

# Create data storage lists 
# Regression model storage list
reg_results_list <- vector(mode = "list", length = length(market_list))
names(reg_results_list) <- keys
# Rates storage lists
rates_list <- vector(mode = "list", length = length(market_list))
rates_indx_list <- vector(mode = "list", length = length(market_list))
names(rates_list) <- keys
names(rates_indx_list) <- keys

# Loop estimates estudy ols objects
# dependency of para and nonpara tests
for (i in 1:length(market_list))
{
  tryCatch({
    print(paste("Getting rates from prices for", keys[[i]]))
    rates <- get_rates_from_prices(stock_list[[i]], 
                                   quote = "Close", 
                                   multi_day = TRUE, 
                                   compounding = "continuous")
    
    rates_indx <- get_rates_from_prices(market_list[[i]], 
                                        quote = "Close", 
                                        multi_day = TRUE, 
                                        compounding = "continuous")
    
    # FIX DTYPES OF COLUMN PRIOR TO TESTING
    rates <- transform.data.frame(rates, date = as.Date(date))
    rates_indx <- transform.data.frame(rates_indx, date = as.Date(date))
    print("Done. Applying single-index market model.")
    
    # apply single-index market model to get ARs
    securities_returns <- apply_market_model(rates = rates,
                                             regressor = rates_indx,
                                             same_regressor_for_all = TRUE,
                                             market_model = "sim",
                                             estimation_method = "ols",
                                             estimation_start = as.Date("2019-04-01"),
                                             estimation_end = as.Date("2020-03-13"))
    
    print("Done. Storing rates and market-models.")
    # Store results for later recording
    rates_list[[keys[[i]]]] <- rates
    rates_indx_list[[keys[[i]]]] <- rates_indx
    reg_results_list[[keys[[i]]]] <- securities_returns
    # Explicit memory cleanup
    rm(rates)
    rm(rates_indx)
    rm(securities_returns)
  }, error = function(e)
  {
    message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
  })
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# 3. Wrangle the models so that it is organised according to sector. ####
# Load Sector Identification data
cd_sec <- 'C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/sectors/icb_stocks_clean.xlsx'
sector_data <- as.list(readxl::read_xlsx(cd_sec))

# Specify patterns
pattern_list <- c(" ", "/", "-", "\\*", "&")

# Remove NAs and fix syntax compatibility
sector_data <- sector_data %>%
  lapply(stringr::str_replace_all,
         pattern = paste0(pattern_list,
                          collapse = "|"),
         replacement = ".") %>%
  lapply(stringr::str_replace_all,
         pattern = ",",
         replacement = "") %>%
  lapply(stringr::str_replace_all,
         pattern = "NA",
         replacement = NA_character_) %>% 
  lapply(fix_digit_names, "X")

# Make syntactically compatible
names(sector_data) <- lapply(names(sector_data), gsub, pattern = " ", replacement = ".")

# reformat as data.frame and remove duplicates
sector_data <- as.data.frame(sector_data) %>% 
  dplyr::distinct(Ticker,.keep_all = TRUE) %>% 
  na.omit

# Create new storage lists
industry <- vector(mode="list",
                   length=length(unique(sector_data$ICB.Industry.Name)))
names(industry) <- unique(sector_data$ICB.Industry.Name)

supersector <- vector(mode="list",
                      length=length(unique(sector_data$ICB.Supersector.Name)))
names(supersector) <- unique(sector_data$ICB.Supersector.Name)

sector <- vector(mode="list",
                 length=length(unique(sector_data$ICB.Sector.Name)))
names(sector) <- unique(sector_data$ICB.Sector.Name)

subsector <- vector(mode="list",
                    length=length(unique(sector_data$ICB.Subsector.Name)))
names(subsector) <- unique(sector_data$ICB.Subsector.Name)

# Plan for reogranisation process
# loop with conditional to test if ticker in sectors is in stock_list
# sectors$Ticker[i] != %in%
# Perhaps it is best to subset all the stock information of all stocks in 'remainder
# then reorganise on the basis of the remainder


# Lists needed for recording of results later
# abnormal return storage list
ar_test_results_list <- vector(mode = "list", length = length(market_list))
names(ar_test_results_list) <- keys
# cumulative abnormal return storage list
car_test_results_list <- vector(mode = "list", length = length(market_list))
names(car_test_results_list) <- keys