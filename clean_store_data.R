# 2. Data Loading ##### 
# Get directories of files
print("Fetching data...")
start_time <- Sys.time()

cd <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/daily/cleaned/es_format/"
files <- c("stock_data_column-wise.csv", "market-index_data_column-wise.csv")
d_m <- "market_indices"
d_s <- "all_stocks"

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
# df2 <- subset(df2, rownames(df2) > as.Date("2019-03-29"))
# df3 <- subset(df3, rownames(df3) > as.Date("2019-03-29"))
# 
# df2 <- subset(df2, rownames(df2) < as.Date("2020-05-01"))
# df3 <- subset(df3, rownames(df3) < as.Date("2020-05-01"))

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

cd_dict = "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/index_member_dict_fixed.xlsx"
cd_problem_stocks = "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/problem_id_s.txt"
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

original_number_of_stocks <- length(unlist(name_list))
long_name_list <- unlist(name_list)
long_name_list_mindu <- name_list[(names(name_list) != "INDU.Index") & (names(name_list) !="CAC.Index")] %>% unlist

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")

# Check for duplicates
if (anyDuplicated(long_name_list_mindu) != 0) {
  dupe_vec <- long_name_list_mindu %>% duplicated 
  cat("There are", sum(as.numeric(dupe_vec)), "duplicate names.")
  dupe_locs <- which(dupe_vec == TRUE)
  ori_vec <- long_name_list_mindu %>% duplicated(fromLast = TRUE)
  ori_locs <- which(ori_vec == TRUE)
  cat("\n", dupe_locs)
} else {
  cat("No duplicates found in", length(long_name_list_mindu), "items")
}
# long_name_list[dupe_locs]
# long_name_list[ori_locs]
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
for (i in 1:length(market_list)) {
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

store_results2(market_list,
               directory = paste0(d_store,
                                  "market_indices/"),
               type = "data",
               rowNames = FALSE)
write.csv(keys,
          file = paste0(d_store,
                        "market_indices/market_names.csv"),
          row.names = FALSE)

## NOTE: THIS DOES NOT CLEAN THE STOCKS EFFECTIVELY WITHOUT THE SLICING
# for the STOCKS (market-constituent) data
for (i in 1:length(stock_list)) {
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
