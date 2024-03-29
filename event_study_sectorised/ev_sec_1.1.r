# Script will perform Sectorised Case-study 
# DATA WRANGLING: CLEANING #### 
# 0. User defined functions and package (library) imports ####
print("Loading libraries and user-defined functions.")
start_time <- Sys.time()

library(magrittr)
library(beepr)

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
  # Returns the name of whatever you pass as a character string 
  # Primary purpose is to get string representation of function and variable names 
  # Added benefit is anything passed as x will come out as 'x'
  deparse(substitute(x))
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")


# SOURCE "apply_market_model_gls.R"####
source("C:/Users/Keegan/Desktop/Repository/@ Development/estudy2_gls/R/apply_market_model_gls.R")
# SOURCE "development_aid_functions.R" ####
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")
# SOURCE: "reconfig.R" ####
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/reconfig.R")
# SOURCE: "bday_windows.R" & "event_spec.R" ####
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/bday_windows.R")
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/event_spec.R")

# 1. Parameters ####
# for weekdays
j.cal.uni <- rjson::fromJSON(file="C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/calendars/weekdays.json")
create.calendar(j.cal.uni$name,
                holidays = j.cal.uni$holidays,
                weekdays = j.cal.uni$weekdays,
                start.date = as.Date("2018-01-01"),
                end.date = as.Date("2020-12-31"),
                adjust.from = j.cal.uni$adjust.from,
                adjust.to = j.cal.uni$adjust.to,
                financial = j.cal.uni$financial)

# list to store all event params
all_events <- vector(mode = 'list', length = 4)

all_events[[1]] <- event_spec(
  "event1",
  grouping = "meta",
  edate = as.Date("2020-01-13"),
  bounds = c(-5, 5),
  est_len = 250,
  calendar = "weekdays"
)
all_events[[2]] <- event_spec(
  "event2",
  grouping = "meta",
  edate = as.Date("2020-01-30"),
  bounds = c(-5, 5),
  est_len = 250,
  calendar = "weekdays"
)
all_events[[3]] <- event_spec(
  "event3",
  grouping = "meta",
  edate = as.Date("2020-02-24"),
  bounds = c(-5, 5),
  est_len = 250,
  calendar = "weekdays"
)
all_events[[4]] <- event_spec(
  "event4",
  grouping = "meta",
  edate = as.Date("2020-03-09"),
  bounds = c(-5, 5),
  est_len = 250,
  calendar = "weekdays"
)

all_events <- all_events %>% set_names(c("event1","event2","event3","event4"))

# 2. Data Loading ##### 
# Get directories of files
print("Fetching data...")
start_time <- Sys.time()

cd <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/daily/cleaned/es_format/"
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

write.table(keys,
            file = "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/id/geographic_region/market_names.txt",
            row.names = FALSE,
            col.names = FALSE)

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

# CHECK CLEANING HAPPENED
if ((identical(market_list, market_list_copy) == TRUE) & (identical(stock_list, stock_list_copy) == TRUE)) {
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n", "1.", name_as_string(market_list)),
          "\n", "2. ", name_as_string(stock_list))
} else if (identical(market_list, market_list_copy) == TRUE) {
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n", "1.", name_as_string(market_list)))
} else if (identical(stock_list, stock_list_copy) == TRUE) {
  message(cat("WARNING: Attempted NAN removal has resulted in identical lists.", "\n", "1.", name_as_string(stock_list)))
} else {
  message(cat("NANs have been removed from the following lists", "\n", "1. ", name_as_string(market_list),
              "\n", "2. ", name_as_string(stock_list)))
}
end_time <- Sys.time()
paste("Complete. Time elapsed: ", round(end_time - start_time, digits = 4), "seconds")




# DATA WRANGLING: REARRANGEMENT ####

# 0. Remove unused objects, Removal of duplicates, user defined functions ####
rm(cds, df2, df3, market_list_copy, stock_list_copy, name_dict)
print("Unused objects removed.")
start_time <- Sys.time()
print("Specifying functions.")
# checks the lengths of objects in order to ensure information preservation
length_checker <- function(obj1, obj2, expected_diff = 0, side = "none", mode = "objects") {
  
  len_a <- 0
  len_b <- 0
  
  if ((mode == "objects") == TRUE) {
    len_a = length(obj1)
    len_b = length(obj2)
  } else if ((mode == "lengths") == TRUE) {
    len_a = obj1
    len_b = obj2
  }
  if ((side == "none") == TRUE) {
    if (len_a != len_b) {
      len_diff <- len_a - len_b
      warn1 <- "UNEXPECTED DIFFERENCE IN OBJECT LENGTHS! \n "
      warn2 <-
        as.character(paste("OBJECT 1 IS",
                           len_diff,
                           "LONGER THAN OBJECT 2. "))
      warning(warn1, warn2)
    } else if ((side == "right") == TRUE) {
      if (len_a != (len_b + expected_diff)) {
        len_diff <- len_a - len_b
        warn1 <- "UNEXPECTED DIFFERENCE IN OBJECT LENGTHS! \n "
        warn2 <-
          as.character(paste("OBJECT 1 IS",
                             len_diff,
                             "LONGER THAN OBJECT 2. "))
        warning(warn1, warn2)
      }
    } else if ((side == "left") == TRUE) {
      if ((len_a + expected_diff) != len_b) {
        len_diff <- len_a - len_b
        warn1 <- "UNEXPECTED DIFFERENCE IN OBJECT LENGTHS! \n "
        warn2 <-
          as.character(paste("OBJECT 1 IS",
                             len_diff,
                             "LONGER THAN OBJECT 2. "))
        warning(warn1, warn2)
      }
    }
  }
}

# COUNTER FUNCTION FOR OPERATION OF LOOPS
start_counter <- function(counter_list) {
  # Function replaces each item of list with the number 1
  for(i in 1:length(counter_list)) {
    counter_list[[i]] <- 1
  } 
  invisible(counter_list)
} 

# SUB-LIST CREATION FUNCTION
sub_list <- function(dataa, llist, focus="") {
  # function takes prespecified lists and replaces NULLs with NULL LISTS of required length
  # i.e. makes NULL list of specified length in to list of NULL lists
  # dataa == data.frame of string identification data
  # llist == prespecified list of length required
  # focus == string that is the name of the column selected from the data
  iters <- 1:length(llist)
  lens <- llist
  for (i in iters) {
    lens[[i]] <- sum(dataa[[focus]] == names(lens)[i])
  }
  for (i in iters) {
    vec <- vector(mode="list", length = lens[[names(llist)[i]]])
    names(vec) <- 
      llist[[names(llist)[i]]] <- vec
    rm(vec)
  }
  rm(lens)
  invisible(llist)
} 

# CHECK TYPES OF OBJECT
check_types <- function(object) {
  cat("typeof:", typeof(object), "\n")
  cat("class:", class(object), "\n")
  cat("mode:", mode(object), "\n")
} 

print("Complete. Removing duplicate stocks from stock-data.")
# REMOVE MARKETS WHERE DUPLICATES RESIDE
len1 <- length(unlist(stock_list, recursive = FALSE))
dupes_num <- length(stock_list[(names(stock_list) != "INDU.Index")]) + 
  length(stock_list[(names(stock_list) != "CAC.Index")])
keys <- keys[(keys != "INDU.Index") & (keys != "CAC.Index")]
market_list <-
  market_list[(names(market_list) != "INDU.Index") &
                (names(market_list) != "CAC.Index")]
stock_list <-
  stock_list[(names(stock_list) != "INDU.Index") &
               (names(stock_list) != "CAC.Index")]
len2 <- length(unlist(stock_list, recursive = FALSE))

cat(
  "The following market's data was removed due to duplicates in other indices: ",
  "\n",
  1,
  "INDU.Index",
  "\n",
  2,
  "CAC.Index",
  "\n\n",
  "A total of",
  len1 - len2,
  "stocks were removed."
)

length_checker(len1, len2, expected_diff = dupes_num, side = "right", mode = "length")

end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")


# START OF META-FOR LOOP ####

for (event_number in seq_along(all_events)) {
  tryCatch({
    
    # CREATION OF OUTER-LOOP-VARIABLES
    EVENT_START <- all_events[[event_number]]$event_window[[1]]
    EVENT_END <-
      all_events[[event_number]]$event_window[[length(all_events[[event_number]]$event_window)]]
    
    ESTIMATION_START <-
      all_events[[event_number]]$estimation_window[[1]]
    ESTIMATION_END <-
      all_events[[event_number]]$estimation_window[[length(all_events[[event_number]]$estimation_window)]]
    
    # 1. OLS/GLS model creation and storage loop ####
    # Creates OLS models on their market specified indices
    
    # Stores models for reorganisation
    start_time <- Sys.time()
    
    # Create data storage lists
    # Regression model storage list
    reg_results_list <-
      vector(mode = "list", length = length(market_list))
    names(reg_results_list) <- keys
    # Rates storage lists
    rates_list <- vector(mode = "list", length = length(market_list))
    rates_indx_list <-
      vector(mode = "list", length = length(market_list))
    names(rates_list) <- keys
    names(rates_indx_list) <- keys
    
    removed_ticks <- vector(mode = "list", length = length(market_list))
    
    # Loop estimates estudy ols/gls objects
    # dependency of para and nonpara tests
    removed <- 0
    for (i in 1:length(market_list)) {
      tryCatch({
        print(paste("Getting rates from prices for", keys[[i]]))
        r8tes <- estudy2::get_rates_from_prices(
          stock_list[[i]],
          quote = "Close",
          multi_day = TRUE,
          compounding = "continuous"
        )
        
        r8tes_indx <-
          estudy2::get_rates_from_prices(
            market_list[[i]],
            quote = "Close",
            multi_day = TRUE,
            compounding = "continuous"
          )
        
        # This loop checks if the values in the columns of 'r8tes' are NOT INVALID
        # If tests eval TRUE, then the offending column's name is stored
        # After the loop is complete, all offending columns are removed
        to_remove <- vector(mode = "list", length = length(r8tes))
        test_rates <- r8tes[-1]
        for (j in seq_along(test_rates)) {
          rn <- names(test_rates)[[j]]
          if (all(test_rates[[rn]] == 0)) {
            to_remove[[j]] <- rn
            
          } else if (sum(test_rates[[j]][100:length(test_rates[[j]])])==0) {
            to_remove[[j]] <- rn
            
          } else if (all(is.na(test_rates[[rn]]))) {
            to_remove[[j]] <- rn
            
          } else if (all(is.null(test_rates[[rn]]))) {
            to_remove[[j]] <- rn
          }
        }
        to_remove <- unlist(to_remove)
        r8tes <- r8tes[, !names(r8tes) %in% to_remove]
        
        # FIX DTYPES OF COLUMN PRIOR TO TESTING
        r8tes <- transform.data.frame(r8tes, date = as.Date(date))
        r8tes_indx <-
          transform.data.frame(r8tes_indx, date = as.Date(date))
        print("Done. Applying single-index market model.")
        
        # apply single-index market model to get ARs
        reg_model_results <-
          apply_market_model.data.frame(
            rates = r8tes,
            regressor = r8tes_indx,
            same_regressor_for_all = TRUE,
            market_model = "sim",
            estimation_method = "gls",
            estimation_start = as.Date(ESTIMATION_START),
            estimation_end = as.Date(ESTIMATION_END)
          )
        
        print("Done. Storing rates and market-models.")
        # Store results for later recording
        rates_list[[keys[[i]]]] <- r8tes
        rates_indx_list[[keys[[i]]]] <- r8tes_indx
        reg_results_list[[keys[[i]]]] <- reg_model_results
        # Record name data in list for later referencing
        temp_keys <- names(r8tes)
        names(reg_results_list[[keys[[i]]]]) <- temp_keys[-1]
        removed_ticks[[i]] <- to_remove
        removed <- removed + 1
        # Explicit memory cleanup
        rm(r8tes)
        rm(r8tes_indx)
        rm(reg_model_results)
        rm(temp_keys)
        rm(to_remove)
      }, error = function(e)
      {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
      })
    }
    # check lengths
    length_checker(
      unlist(stock_list, recursive = FALSE),
      names(unlist(reg_results_list, recursive = FALSE)),
      expected_diff = removed,
      side = "right"
    )
    
    dupe_yesno <- 0
    # Check for duplicates, piecewise within geographies
    for (i in 1:length(reg_results_list)) {
      check_dupes <- reg_results_list[[i]] %>% names %>% anyDuplicated
      if (check_dupes != 0) {
        cat(
          "There are duplicates in market-index: ",
          names(reg_results_list)[[i]],
          "\n",
          "N = ",
          i
        )
      }
      if ((i == length(reg_results_list)) == TRUE) {
        cat("No duplicates found for",
            length(unlist(reg_results_list, recursive = FALSE)),
            "items",
            "\n")
      }
    }
    if (dupe_yesno == 1) {
      dupe_vec <- reg_results_list[[i]] %>% names %>% duplicated
      cat("There are ", sum(as.numeric(dupe_vec)), "duplicate names.")
      dupe_locs <- which(dupe_vec == TRUE)
      ori_vec <- long_name_list %>% duplicated(fromLast = TRUE)
      ori_locs <- which(ori_vec == TRUE)
      cat("\n", dupe_locs)
    }
    
    end_time <- Sys.time()
    paste("OLS model creation and storage complete. Time elapsed: ")
    print(round(end_time - start_time, digits = 4))

    # 2. Load Sector Identification data ####
    start_time <- Sys.time()
    print("Loading ICB classification data.")
    cd_sec <-
      'C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/sectors/icb_stocks_clean.xlsx'
    sector_data <- as.list(readxl::read_xlsx(cd_sec))
    
    # Specify patterns
    pattern_list <- c(" ", "/", "-", "\\*", "&")
    
    # Remove NAs and fix syntax compatibility
    sector_data <- sector_data %>%
      lapply(
        stringr::str_replace_all,
        pattern = paste0(pattern_list,
                         collapse = "|"),
        replacement = "."
      ) %>%
      lapply(stringr::str_replace_all,
             pattern = ",",
             replacement = "") %>%
      lapply(stringr::str_replace_all,
             pattern = "NA",
             replacement = NA_character_) %>%
      lapply(fix_digit_names, "X")
    
    # Make syntactically compatible
    names(sector_data) <-
      lapply(names(sector_data),
             gsub,
             pattern = " ",
             replacement = ".")
    
    # reformat as data.frame and remove duplicates
    sector_data <- as.data.frame(sector_data) %>%
      dplyr::distinct(Ticker, .keep_all = TRUE) %>%
      na.omit
    
    end_time <- Sys.time()
    paste("Complete. Time elapsed: ",
          round(end_time - start_time, digits = 4),
          "seconds")
    
    # 3. Reference REGEX specification for name-string cleaning ####
    start_time <- Sys.time()
    
    # Construct regex patterns for later
    pat <- names(reg_results_list)
    pat <- pat %>%
      lapply(stringr::str_remove_all,
             "\\.Index\\.?") %>%
      unlist
    
    for (i in 1:length(pat)) {
      pat[[i]] <- paste0("^", pat[[i]], "\\.Index\\.?")
    }
    print("REGEX patterns constructed for name (string) cleaning")
    end_time <- Sys.time()
    paste("Time elapsed: ")
    print(round(end_time - start_time, digits = 4))
    # Unlist the 'list of lists of lists' into a 'list of lists'
    lol <- unlist(reg_results_list, recursive = FALSE)

    # 4. Fix names using regex ####
    start_time <- Sys.time()
    print('Cleaning string data.')
    # Fix names using regex, whilst maintaining order
    fixed_names <- names(lol) %>%
      lapply(stringr::str_remove_all,
             pattern = paste0(pat,
                              collapse = "|")) %>%
      unlist
    
    # check lengths
    length_checker(names(unlist(reg_results_list, recursive = FALSE)), lol, side = "right")
    length_checker(names(lol), fixed_names)
    
    # Check for duplicates
    if (anyDuplicated(fixed_names) != 0) {
      dupe_vec <- fixed_names %>% duplicated
      cat("There are ", sum(as.numeric(dupe_vec)), "duplicate names.")
      dupe_locs <- which(dupe_vec == TRUE)
      ori_vec <- fixed_names %>% duplicated(fromLast = TRUE)
      ori_locs <- which(ori_vec == TRUE)
      cat("\n", dupe_locs)
    }
    end_time <- Sys.time()
    paste("Complete. Time elapsed: ",
          round(end_time - start_time, digits = 4),
          "seconds")
    # Rename items in list
    names(lol) <- fixed_names
    
    # 5 Subset ID and Model data in order for inner join match ####
    start_time <- Sys.time()
    print("Subsetting OLS model data according to inner join with ICB identification data.")
    # Get reference names
    usable_ticks <- intersect(sector_data[["Ticker"]], fixed_names)
    removed_ticks <- unlist(removed_ticks)
    usable_ticks <- usable_ticks[!(usable_ticks %in% removed_ticks)]
    
    len3 <- nrow(sector_data)
    # Remove irrelevant names
    # For Models # is.element(names(lol),usable_ticks)
    lol_idx <- which(names(lol) %in% usable_ticks)
    lol <- lol[lol_idx]
    # For sector classification data
    sec_idx <- which(sector_data[, 1] %in% usable_ticks)
    sector_data <- sector_data[sec_idx, ]
    
    length_checker(
      len3,
      nrow(sector_data),
      expected_diff = 466,
      side = "right",
      mode = "lengths"
    )
    
    end_time <- Sys.time()
    paste("Complete. Time elapsed: ",
          round(end_time - start_time, digits = 4),
          "seconds")
    
    # 6. Pre-allocate objects and variables for sector wrangling ####
    start_time <- Sys.time()
    print("Creating storage objects for model regrouping.")
    
    # Create new storage lists
    # need to pre-allocate the length of the subcomponents of the lists
    industry <- vector(mode = "list",
                       length = length(unique(sector_data$ICB.Industry.Name)))
    names(industry) <- unique(sector_data$ICB.Industry.Name)
    indu_i <- industry # copy list for later counter
    industry <-
      sub_list(sector_data, industry, focus = "ICB.Industry.Name")
    
    supersector <- vector(mode = "list",
                          length = length(unique(sector_data$ICB.Supersector.Name)))
    names(supersector) <- unique(sector_data$ICB.Supersector.Name)
    supe_i <- supersector # copy list for later counter
    
    supersector <-
      sub_list(sector_data, supersector, focus = "ICB.Supersector.Name")
    
    sector <- vector(mode = "list",
                     length = length(unique(sector_data$ICB.Sector.Name)))
    names(sector) <- unique(sector_data$ICB.Sector.Name)
    sect_i <- sector # copy list for later counter
    sector <- sub_list(sector_data, sector, focus = "ICB.Sector.Name")
    
    subsector <- vector(mode = "list",
                        length = length(unique(sector_data$ICB.Subsector.Name)))
    names(subsector) <- unique(sector_data$ICB.Subsector.Name)
    subs_i <- subsector # copy list for later counter
    subsector <-
      sub_list(sector_data, subsector, focus = "ICB.Subsector.Name")
    
    print("Creation of Storage Objects complete.")
    end_time <- Sys.time()
    paste("Time elapsed: ")
    round(end_time - start_time, digits = 4)
    
    # find total number of qualifying shares for Industry & Supersector
    industry_stocks_total <-
      length(unique(sector_data$ICB.Industry.Name))
    paste("Total industries:", industry_stocks_total)
    supersector_stock_total <-
      length(unique(sector_data$ICB.Supersector.Name))
    paste("Total supersectors:", supersector_stock_total)
    
    
    start_time <- Sys.time()
    print("Reconfiguring storage objects to contain specified named-references.")
    # START COUNTERS AT 1
    indu_i <- start_counter(indu_i)
    supe_i <- start_counter(supe_i)
    sect_i <- start_counter(sect_i)
    subs_i <- start_counter(subs_i)
    
    # PRE-ALLOCATION LOOP: CREATION OF NAMED REFERENCES
    for (i in 1:nrow(sector_data)) {
      # each row of 'sector_data' contains a single stock's ICB information
      # this loop is supposed to go over the 'sector_data' and carve it up line-by-line
      # then checks where that stock's name is supposed to go
      # It then allocates the ticker of that stock in the sub-list as a name
      # to the pre-specified list of the correct length (see 'sub_list()')
      
      # CARVE UP INTO CONSTITUENTS
      tick <- sector_data[i, 1] # ticker
      indu <- sector_data[i, 2] # industry
      supe <- sector_data[i, 3] # supersector
      sect <- sector_data[i, 4] # sector
      subs <- sector_data[i, 5] # subsector
      
      # ALLOCATE TICKERS (ID strings) to respective locations
      if (sector_data[i, 2] == indu) {
        # INDUSTRY
        industry[[indu]][[indu_i[[indu]]]] <- tick
        names(industry[[indu]])[[indu_i[[indu]]]] <- tick
        indu_i[[indu]] <- indu_i[[indu]] + 1
      } else {
        
      }
      if (sector_data[i, 3] == supe) {
        # SUPERSECTOR
        supersector[[supe]][[supe_i[[supe]]]] <- tick
        names(supersector[[supe]])[[supe_i[[supe]]]] <- tick
        supe_i[[supe]] <- supe_i[[supe]] + 1
      } else {
        
      }
      if (sector_data[i, 4] == sect) {
        # SECTOR
        sector[[sect]][[sect_i[[sect]]]] <- tick
        names(sector[[sect]])[[sect_i[[sect]]]] <- tick
        sect_i[[sect]] <- sect_i[[sect]] + 1
      } else {
        
      }
      if (sector_data[i, 5] == subs) {
        # SUBSECTOR
        subsector[[subs]][[subs_i[[subs]]]] <- tick
        names(subsector[[subs]])[[subs_i[[subs]]]] <- tick
        subs_i[[subs]] <- subs_i[[subs]] + 1
      }
    }
    print("Named-reference reconfiguration of storage objects complete.")
    end_time <- Sys.time()
    paste("Time elapsed: ",
          round(end_time - start_time, digits = 4),
          "seconds")
    
    # 7.1 Allocate data to pre-allocated storage objects ####
    # ALLOCATE REGRESSION MODEL DATA AND INFO TO PREALLOCATED LOCATION
    start_time <- Sys.time()
    print("Regrouping calculated models according ICB classifications. Allocating.")
    
    for (i in 1:length(lol)) {
      tryCatch({
        # Get ID and classification for matching purposes
        nam <- names(lol)[[i]]
        if ((nam %in% sector_data[, 1]) == FALSE) {
          next
        }
        # Remove old objects
        if (exists("row_slice") == TRUE) {
          rm(row_slice)
        }
        if (exists("tick") == TRUE) {
          rm(tick)
        }
        if (exists("indu") == TRUE) {
          rm(indu)
        }
        if (exists("supe") == TRUE) {
          rm(supe)
        }
        if (exists("sect") == TRUE) {
          rm(sect)
        }
        if (exists("subs") == TRUE) {
          rm(subs)
        }
        # Slice sector info for matching
        row_slice <- sector_data[sector_data[, 1] == nam, ]
        tick <- row_slice[["Ticker"]]
        indu <- row_slice[["ICB.Industry.Name"]]
        supe <- row_slice[["ICB.Supersector.Name"]]
        sect <- row_slice[["ICB.Sector.Name"]]
        subs <- row_slice[["ICB.Subsector.Name"]]
        # print(i)
        # ALLOCATE MODELS VIA REFERENCING
        industry[[indu]][[tick]] <- lol[[tick]]
        supersector[[supe]][[tick]] <- lol[[tick]]
        sector[[sect]][[tick]] <- lol[[tick]]
        subsector[[subs]][[tick]] <- lol[[tick]]
        
      }, error = function(e)
      {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
      })
    }
    end_time <- Sys.time()
    paste(
      "Model allocation complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds"
    )
    # 7.2 Reconfigure allocated regression models data class == 'returns' ####
    industry_names <- names(industry)
    supersector_names <- names(supersector)
    print("Reconfiguring allocated INDUSTRY data.")
    start_time <- Sys.time()
    for (i in seq_along(industry)) {
      indu <- industry_names[[i]]
      industry[[indu]] <- reconfig(industry[[indu]], dropNA = FALSE)
    }
    print("Complete. Reconfiguring allocated SUPERSECTOR data.")
    for (i in seq_along(supersector)) {
      supe <- supersector_names[[i]]
      supersector[[supe]] <- reconfig(supersector[[supe]], dropNA = FALSE)
    }
    print('Complete.')
    end_time <- Sys.time()
    print(end_time-start_time)
    
    # Find total number of stocks per industry & supersector category ####
    count_indu <- vector(mode = "list",
                         length = length(unique(sector_data$ICB.Industry.Name))) %>%
      set_names(unique(sector_data$ICB.Industry.Name))
    count_supe <- vector(mode = "list",
                         length = length(unique(sector_data$ICB.Supersector.Name))) %>%
      set_names(unique(sector_data$ICB.Supersector.Name))
    indu_g <- unique(sector_data$ICB.Industry.Name) %>% as.character()
    for (i in seq_along(indu_g)) {
      group <- indu_g[[i]]
      count_indu[[group]] <- length(industry[[group]])
    }
    
    supe_g <-
      unique(sector_data$ICB.Supersector.Name) %>% as.character()
    for (i in seq_along(supe_g)) {
      group <- supe_g[[i]]
      count_supe[[group]] <- length(supersector[[group]])
    }
    
    count_indu <-
      count_indu %>% as.data.frame %>% t %>% set_colnames(c("Count"))
    count_supe <-
      count_supe %>% as.data.frame %>% t %>% set_colnames(c("Count"))
    
    d_tables <-
      "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/results_presentation/tables"
    
    write.csv(count_indu,
              file = paste0(d_tables,
                            "/table_3a_industry_count.csv"))
    write.csv(count_supe,
              file = paste0(d_tables,
                            "/table_3b_supersector_count.csv"))
    
    # Wrangle data to make tables where tickers are sorted by the indu & supe groupings 
    # INDUSTRY
    industry_components <- industry
    # make list of data.frames containing grouped ticker data
    for (i in seq_along(industry_components)) {
      industry_components[[i]] <- industry[[i]] %>%
        names %>%
        as.data.frame() %>%
        set_colnames(names(industry)[[i]]) %>%
        umx::umx_pad(n = max(count_indu))
    }
    # create empty data.frame with right colnames and right nrow
    df_i <- as.data.frame(
      matrix(NA,
             nrow = max(count_indu),
             ncol = length(unique(sector_data$ICB.Industry.Name))
             )
      ) %>%
      set_colnames(unique(sector_data$ICB.Industry.Name))
    # replace empty columns with data
    for (i in seq_along(industry_components)) {
      tic <- unique(sector_data$ICB.Industry.Name)[[i]]
      df_i[[tic]] <-
        industry_components[[tic]] %>% unlist %>% as.character()
    }
    write.csv(x = df_i,
              file = "C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/sample_data/table_4a_industry_tickers.csv",
              na = "")
    
    # SUPERSECTOR
    supersector_components <- supersector
    # make list of data.frames containing grouped ticker data
    for (i in seq_along(supersector_components)) {
      supersector_components[[i]] <- supersector[[i]] %>%
        names %>%
        as.data.frame() %>%
        set_colnames(names(supersector)[[i]]) %>%
        umx::umx_pad(n = max(count_supe))
    }
    # create empty data.frame with right colnames and right nrow
    df_s <- as.data.frame(matrix(
      NA,
      nrow = max(count_supe),
      ncol = length(unique(sector_data$ICB.Supersector.Name))
    )) %>%
      set_colnames(unique(sector_data$ICB.Supersector.Name))
    # replace empty columns with data
    for (i in seq_along(supersector_components)) {
      tic <- unique(sector_data$ICB.Supersector.Name)[[i]]
      df_s[[tic]] <-
        supersector_components[[tic]] %>% unlist %>% as.character()
    }
    write.csv(x = df_s,
              file = "C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/sample_data/table_4b_supersector_tickers.csv",
              na = "")
    
    # 8. Preparation for calculation of test results ####
    start_time <- Sys.time()
    print("Applying event-study statistical process to stored models and recording results.")
    # PREALLOCATE STORAGE LIST
    # abnormal return
    ar_industry <- vector(mode = "list", length = length(industry))
    names(ar_industry) <- names(industry)
    ar_supersector <-
      vector(mode = "list", length = length(supersector))
    names(ar_supersector) <- names(supersector)
    ar_sector <- vector(mode = "list", length = length(sector))
    names(ar_sector) <- names(sector)
    ar_subsector <- vector(mode = "list", length = length(subsector))
    names(ar_subsector) <- names(subsector)
    
    # cumulative abnormal return
    car_industry <- vector(mode = "list", length = length(industry))
    names(car_industry) <- names(industry)
    car_supersector <-
      vector(mode = "list", length = length(supersector))
    names(car_supersector) <- names(supersector)
    car_sector <- vector(mode = "list", length = length(sector))
    names(car_sector) <- names(sector)
    car_subsector <- vector(mode = "list", length = length(subsector))
    names(car_subsector) <- names(subsector)
    
    
    # 9. Loops that apply statistical Estudy tests over the large dataset for ICB groupings: ####
    
    # 9.1 INDUSTRY ####
    start_time <- Sys.time()
    cat("Testing INDUSTRY.", "\n")
    for (i in 1:length(industry)) {
      tryCatch({
        print("Applying parametric and non-parametric tests to abnormal returns.")
        
        keyss <- names(industry)
        
        securities_returns <- industry[[i]]
        
        if (length(securities_returns) < 2) {
          next
        }
        
        # ABNORMAL RETURN TESTS
        # Parametric tests
        ar_para <- data.frame(
          estudy2::parametric_tests(
            list_of_returns = securities_returns,
            event_start = as.Date(EVENT_START),
            event_end = as.Date(EVENT_END)
          )
        )
        # Non-parametric tests
        ar_non_para <- data.frame(
          estudy2::nonparametric_tests(
            list_of_returns = securities_returns,
            event_start = as.Date(EVENT_START),
            event_end = as.Date(EVENT_END)
          )
        )
        # CUMULATIVE ABNORMAL RETURN TESTS
        # Parametric tests
        car_para <- data.frame(
          estudy2::car_parametric_tests(
            list_of_returns = securities_returns,
            car_start = as.Date(EVENT_START),
            car_end = as.Date(EVENT_END)
          )
        )
        # Non-parametric tests
        car_non_para <- data.frame(
          estudy2::car_nonparametric_tests(
            list_of_returns = securities_returns,
            car_start = as.Date(EVENT_START),
            car_end = as.Date(EVENT_END)
          )
        )
        print(paste("Merging results.", keyss[[i]], "n = ", i))
        # Stage results for storage
        ar_results <-
          data.frame(merge(ar_para, ar_non_para, by = "date"))
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
        ar_industry[[keyss[[i]]]] <- ar_results
        car_industry[[keyss[[i]]]] <- car_results
        # Explicit memory cleanup
        rm(ar_results,
           car_results,
           ar_para,
           ar_non_para,
           car_para,
           car_non_para)
      }, error = function(e) {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i , "\n"))
      })
    }
    end_time <- Sys.time()
    paste("Complete. Time elapsed: ",
          round(end_time - start_time, digits = 4),
          "seconds")
    
    # 9.2 SUPERSECTOR ####
    start_time <- Sys.time()
    cat("Testing SUPERSECTORs.", "\n")
    for (i in 1:length(supersector)) {
      tryCatch({
        print("Applying parametric and non-parametric tests to abnormal returns.")
        
        securities_returns <- supersector[[i]]
        
        if (length(securities_returns) < 2) {
          next
        }
        
        # ABNORMAL RETURN TESTS
        # Parametric tests
        ar_para <- data.frame(
          estudy2::parametric_tests(
            list_of_returns = securities_returns,
            event_start = as.Date(EVENT_START),
            event_end = as.Date(EVENT_END)
          )
        )
        # Non-parametric tests
        ar_non_para <- data.frame(
          estudy2::nonparametric_tests(
            list_of_returns = securities_returns,
            event_start = as.Date(EVENT_START),
            event_end = as.Date(EVENT_END)
          )
        )
        # CUMULATIVE ABNORMAL RETURN TESTS
        # Parametric tests
        car_para <- data.frame(
          estudy2::car_parametric_tests(
            list_of_returns = securities_returns,
            car_start = as.Date(EVENT_START),
            car_end = as.Date(EVENT_END)
          )
        )
        # Non-parametric tests
        car_non_para <- data.frame(
          estudy2::car_nonparametric_tests(
            list_of_returns = securities_returns,
            car_start = as.Date(EVENT_START),
            car_end = as.Date(EVENT_END)
          )
        )
        print(paste("Merging results.", supersector_names[[i]], "n = ", i))
        # Stage results for storage
        ar_results <-
          data.frame(merge(ar_para, ar_non_para, by = "date"))
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
        ar_supersector[[supersector_names[[i]]]] <- ar_results
        car_supersector[[supersector_names[[i]]]] <- car_results
        # Explicit memory cleanup
        rm(ar_results,
           car_results,
           ar_para,
           ar_non_para,
           car_para,
           car_non_para)
      }, error = function(e) {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i , "\n"))
      })
    }
    end_time <- Sys.time()
    paste("Complete. Time elapsed: ",
          round(end_time - start_time, digits = 4),
          "seconds")
    
    # 10. Recording of results in new '.csv' data-files #####
    start_time <- Sys.time()
    print("Recording results in prespecified directories.")
    
    # WRITE RESULTS
    cd_base <-
      "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/industry_classification/"
    
    d_ar <- 'ar/'
    d_aar <- 'aar/'
    d_ar_res <- 'ar_res/'
    
    d_car <- 'car/'
    d_caar <- 'caar/'
    d_car_res <- 'car_res/'
    
    d_industry <- 'industry/'
    d_supersector <- 'supersector/'
    
    # Store results of statistical tests ####
    store_results2(
      results = ar_industry,
      directory = paste0(cd_base, d_industry, all_events[[event_number]]$event_name, "/", d_ar_res),
      type = "data"
    )
    store_results2(
      results = car_industry,
      directory = paste0(cd_base, d_industry, all_events[[event_number]]$event_name, "/", d_car_res),
      type = "data"
    )
    store_results2(
      results = ar_supersector,
      directory = paste0(cd_base, d_supersector, all_events[[event_number]]$event_name, "/", d_ar_res),
      type = "data"
    )
    store_results2(
      results = car_supersector,
      directory = paste0(cd_base, d_supersector, all_events[[event_number]]$event_name, "/", d_car_res),
      type = "data"
    )

    end_time <- Sys.time()
    paste("Complete.")
    round(end_time - start_time, digits = 4) %>%  print()
    
    # 11. Process to capture & store ARs and CARs
    
    # 11.1. Pre-allocate objects and variables for sector wrangling ####
    start_time <- Sys.time()
    print("Creating storage objects for model regrouping.")
    
    # Create new storage lists
    # need to pre-allocate the length of the subcomponents of the lists
    # ARs
    ar_data_industry <- make_list(length(unique(sector_data$ICB.Industry.Name)),
                                  unique(sector_data$ICB.Industry.Name))
    indu_i <- ar_data_industry # copy list for later counter
    ar_data_industry <- sub_list(sector_data,
                                 ar_data_industry,
                                 focus = "ICB.Industry.Name")
    
    ar_data_supersector <- make_list(length(unique(sector_data$ICB.Supersector.Name)),
                                     unique(sector_data$ICB.Supersector.Name))
    supe_i <- ar_data_supersector # copy list for later counter
    ar_data_supersector <- sub_list(sector_data,
                                    ar_data_supersector,
                                    focus = "ICB.Supersector.Name")
    
    end_time <- Sys.time()
    print("Creation of Storage Objects complete.")
    round(end_time - start_time, digits = 4) %>%  print()
    
    start_time <- Sys.time()
    print("Reconfiguring storage objects to contain specified named-references.")
    # START COUNTERS AT 1
    indu_i <- start_counter(indu_i)
    supe_i <- start_counter(supe_i)
    
    # PRE-ALLOCATION LOOP: CREATION OF NAMED REFERENCES ####
    for (i in 1:nrow(sector_data)) {
      # each row of 'sector_data' contains a single stock's ICB information
      # this loop is supposed to go over the 'sector_data' and carve it up line-by-line
      # then checks where that stock's name is supposed to go
      # It then allocates the ticker of that stock in the sub-list as a name
      # to the pre-specified list of the correct length (see 'sub_list()')
      
      # CARVE UP INTO CONSTITUENTS
      tick <- sector_data[i, 1] # ticker
      indu <- sector_data[i, 2] # industry
      supe <- sector_data[i, 3] # supersector
      
      # ALLOCATE TICKERS (ID strings) to respective locations
      if (sector_data[i, 2] == indu) {
        # INDUSTRY
        ar_data_industry[[indu]][[indu_i[[indu]]]] <- tick
        names(ar_data_industry[[indu]])[[indu_i[[indu]]]] <- tick
        indu_i[[indu]] <- indu_i[[indu]] + 1
      } else {
        
      }
      if (sector_data[i, 3] == supe) {
        # SUPERSECTOR
        ar_data_supersector[[supe]][[supe_i[[supe]]]] <- tick
        names(ar_data_supersector[[supe]])[[supe_i[[supe]]]] <- tick
        supe_i[[supe]] <- supe_i[[supe]] + 1
      } else {
        
      }
    }
    print("Named-reference reconfiguration of storage objects complete.")
    end_time <- Sys.time()
    paste("Complete.")
    round(end_time - start_time, digits = 4) %>%  print()
    
    # 11.2. Allocate data to pre-allocated storage objects ####
    # ALLOCATE CALCULATED ABNORMAL RETURNS DATA TO PREALLOCATED LOCATION
    start_time <- Sys.time()
    print(
      "Extracting and regrouping calculated abnormal returns according ICB classifications."
    )
    
    for (i in 1:length(lol)) {
      tryCatch({
        # Get ID and classification for matching purposes
        nam <- names(lol)[[i]]
        if ((nam %in% sector_data[, 1]) == FALSE) {
          next
        }
        # Remove old objects
        if (exists("row_slice") == TRUE) {
          rm(row_slice)
        }
        if (exists("tick") == TRUE) {
          rm(tick)
        }
        if (exists("indu") == TRUE) {
          rm(indu)
        }
        if (exists("supe") == TRUE) {
          rm(supe)
        }
        
        # Slice sector info for matching
        row_slice <- sector_data[sector_data[, 1] == nam, ]
        tick <- row_slice[["Ticker"]]
        indu <- row_slice[["ICB.Industry.Name"]]
        supe <- row_slice[["ICB.Supersector.Name"]]
        
        # print(i)
        # ALLOCATE MODELS VIA REFERENCING
        ar_data_industry[[indu]][[tick]] <- lol[[tick]]$abnormal
        ar_data_supersector[[supe]][[tick]] <- lol[[tick]]$abnormal
        
        # Subset and remove the estimation window data from the ARs and CARs
        EVENT_WINDOW_START <-
          all_events[[event_number]]$event_window[[1]]
        EVENT_WINDOW_END <-
          all_events[[event_number]]$event_window[[length(all_events[[event_number]]$event_window)]]
        
        ar_data_industry[[indu]][[tick]] <-
          subset(ar_data_industry[[indu]][[tick]],
                 (
                   zoo::index(ar_data_industry[[indu]][[tick]]) >= as.Date(EVENT_WINDOW_START)
                 ) &
                   (
                     zoo::index(ar_data_industry[[indu]][[tick]]) <= as.Date(EVENT_WINDOW_END)
                   ))
        
        ar_data_supersector[[supe]][[tick]] <-
          subset(ar_data_supersector[[supe]][[tick]],
                 (
                   zoo::index(ar_data_supersector[[supe]][[tick]]) >= as.Date(EVENT_WINDOW_START)
                 ) &
                   (
                     zoo::index(ar_data_supersector[[supe]][[tick]]) <= as.Date(EVENT_WINDOW_END)
                   ))
      }, error = function(e)
      {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
      })
    }
    
    end_time <- Sys.time()
    paste("Complete.")
    round(end_time - start_time, digits = 4) %>%  print()
    
    # 11.3 Reconfigure into merged zoo objects & and calculate CARS
    start_time <- Sys.time()
    industry_names <- names(ar_data_industry)
    supersector_names <- names(ar_data_supersector)
    
    # CARs: INDUSTRY ####
    # AR and CAR calculation
    ar_data_industry_merged <- ar_data_industry
    for (i in seq_along(ar_data_industry)) {
      tryCatch({
        indu <- industry_names[[i]]
        ar_data_industry_merged[[indu]] <- do.call(zoo::merge.zoo,
                                                   ar_data_industry[[indu]]) %>% as.data.frame
      }, error = function(e)
      {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
      })
    }
    car_data_industry_merged <- ar_data_industry_merged
    for (i in seq_along(ar_data_industry_merged)) {
      tryCatch({
        indu <- industry_names[[i]]
        shem_vec <- names(ar_data_industry_merged[[indu]])
        for (j in 1:ncol(ar_data_industry_merged[[indu]])) {
          tryCatch({
            # Get name
            shem <- shem_vec[[j]]
            # Slice out series
            s1 <- ar_data_industry_merged[[indu]][, shem]
            s2 <- s1
            # Accumulate returns, ignoring NAs
            s2[!is.na(s1)] <- cumsum(s2[!is.na(s1)])
            # store returns in order
            car_data_industry_merged[[indu]][, shem] <- s2
            rm(s1, s2)
          }, error = function(e)
          {
            message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
          })
        }
        ar_data_industry_merged[[indu]] <-
          as.data.frame(ar_data_industry_merged[[indu]])
      }, error = function(e)
      {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
      })
    }
    
    # CARs: SUPERSECTOR ####
    # AR and CAR calculation
    ar_data_supersector_merged <- ar_data_supersector
    for (i in seq_along(ar_data_supersector)) {
      tryCatch({
        supe <- supersector_names[[i]]
        ar_data_supersector_merged[[supe]] <- do.call(zoo::merge.zoo,
                                                      ar_data_supersector[[supe]]) %>% as.data.frame
      }, error = function(e)
      {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
      })
    }
    car_data_supersector_merged <- ar_data_supersector_merged
    for (i in seq_along(ar_data_supersector_merged)) {
      tryCatch({
        supe <- supersector_names[[i]]
        shem_vec <- names(ar_data_supersector_merged[[supe]])
        for (j in 1:ncol(ar_data_supersector_merged[[supe]])) {
          tryCatch({
            # Get name
            shem <- shem_vec[[j]]
            # Slice out series
            s1 <- ar_data_supersector_merged[[supe]][, shem]
            s2 <- s1
            # Accumulate returns, ignoring NAs
            s2[!is.na(s1)] <- cumsum(s2[!is.na(s1)])
            # store returns in order
            car_data_supersector_merged[[supe]][, shem] <- s2
            rm(s1, s2)
          }, error = function(e)
          {
            message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
          })
        }
        ar_data_supersector_merged[[supe]] <-
          as.data.frame(ar_data_supersector_merged[[supe]])
      }, error = function(e)
      {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
      })
    }
    end_time <- Sys.time()
    paste("Complete.")
    round(end_time - start_time, digits = 4) %>%  print()

    # Store AR and CAR results ####
    # INDUSTRY
    store_results2(
      results = ar_data_industry_merged,
      directory = paste0(cd_base, d_industry, all_events[[event_number]]$event_name, "/", d_ar),
      type = "data"
    )
    store_results2(
      results = car_data_industry_merged,
      directory = paste0(cd_base, d_industry, all_events[[event_number]]$event_name, "/", d_car),
      type = "data"
    )

    # SUPERSECTOR
    store_results2(
      results = ar_data_supersector_merged,
      directory = paste0(cd_base, d_supersector, all_events[[event_number]]$event_name, "/", d_ar),
      type = "data"
    )
    store_results2(
      results = car_data_supersector_merged,
      directory = paste0(cd_base, d_supersector, all_events[[event_number]]$event_name, "/", d_car),
      type = "data"
    )

    paste("Complete.")
    round(end_time - start_time, digits = 4) %>%  print()
    
    # 11.3. Calculate average abnormal returns ####
    start_time <- Sys.time()
    print("Calculating abnormal returns according ICB classifications.")
    
    # INDUSTRY
    aar_data_industry_merged <- ar_data_industry_merged
    caar_data_industry_merged <- car_data_industry_merged
    for (i in seq_along(ar_data_industry)) {
      indu <- industry_names[[i]]
      aar_data_industry_merged[[indu]] <- rowMeans(ar_data_industry_merged[[indu]],
                                                   na.rm = TRUE) %>%
        as.data.frame() %>%
        set_rownames(row.names(ar_data_industry_merged[[indu]])) %>%
        set_colnames(indu)
      caar_data_industry_merged[[indu]] <-
        cumsum(aar_data_industry_merged[[indu]])
    }
    # SUPERSECTOR
    aar_data_supersector_merged <- ar_data_supersector_merged
    caar_data_supersector_merged <- ar_data_supersector_merged
    for (i in seq_along(ar_data_supersector)) {
      supe <- supersector_names[[i]]
      aar_data_supersector_merged[[supe]] <- rowMeans(aar_data_supersector_merged[[supe]],
                                                      na.rm = TRUE) %>%
        as.data.frame() %>%
        set_rownames(row.names(aar_data_supersector_merged[[supe]])) %>%
        set_colnames(supe)
      caar_data_supersector_merged[[supe]] <-
        cumsum(aar_data_supersector_merged[[supe]])
    }
    
    # Store results ####
    # INDUSTRY 
    store_results2(
      results = aar_data_industry_merged,
      directory = paste0(cd_base, d_industry, all_events[[event_number]]$event_name, "/", d_aar),
      type = "data"
    )
    store_results2(
      results = caar_data_industry_merged,
      directory = paste0(cd_base, d_industry, all_events[[event_number]]$event_name, "/", d_caar),
      type = "data"
    )
    # SUPERSECTOR
    store_results2(
      results = aar_data_supersector_merged,
      directory = paste0(cd_base, d_supersector, all_events[[event_number]]$event_name, "/", d_aar),
      type = "data"
    )
    store_results2(
      results = caar_data_supersector_merged,
      directory = paste0(cd_base, d_supersector, all_events[[event_number]]$event_name, "/", d_caar),
      type = "data"
    )
  },
  error = function(e)
  {
    message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
  })
}
# END OF LOOP ####
paste("Complete.")
round(end_time - start_time, digits = 4) %>%  print()

beep()
print("Script complete.")