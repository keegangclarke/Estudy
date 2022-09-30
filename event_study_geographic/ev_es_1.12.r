# 0. User defined functions and package (library) imports ####
print("Loading libraries and user-defined functions.")
start_time <- Sys.time()

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

# 1. Parameters ####
# list to store all event params
all_events <- vector(mode = 'list', length = 4)

event_spec <- function(name = "",
                       edate = NULL,
                       bounds = NULL,
                       est_len = NULL) {
  if ((name == "") == TRUE) {
    message("Please specify event name as string.")
  } else if (is.null(edate) == TRUE) {
    message("Please specify event date.")
  } else if ((class(edate) != "Date") == TRUE) {
    message("Please ensure event date is 'as.Date'")
  } else if (is.null(bounds) == TRUE) {
    message("Please specify event bounds.")
  } else if (is.null(est_len) == TRUE) {
    message("Please specify estimation length.")
  } else {
    event_window <- seq.Date(from = edate - abs(bounds[1]),
                             to = edate + bounds[2],
                             by = 'day')
    estimation_window <-
      seq.Date(
        from = edate - abs(bounds[1]) - abs(est_len + 1),
        to = edate + bounds[1] - 1 ,
        by = 'day'
      )
    
    event_specification <- vector(mode = 'list', length = 4)
    event_specification[[1]] <- name
    event_specification[[2]] <- edate
    event_specification[[3]] <- event_window
    event_specification[[4]] <- estimation_window
    
    event_specification <-
      setNames(
        event_specification,
        c(
          "event_name",
          "event_date",
          "event_window",
          "estimation_window"
        )
      )
    
    class(event_specification) <- "event_spec"
    return(event_specification)
  }
}

all_events[[1]] <- event_spec(
  "Event1",
  edate = as.Date("2020-01-13"),
  bounds = c(-5, 5),
  est_len = 250
)
all_events[[2]] <- event_spec(
  "Event2",
  edate = as.Date("2020-01-24"),
  bounds = c(-2, 8),
  est_len = 250
)
all_events[[3]] <- event_spec(
  "Event3",
  edate = as.Date("2020-02-24"),
  bounds = c(-1, 9),
  est_len = 250
)
all_events[[4]] <- event_spec(
  "Event4",
  edate = as.Date("2020-03-09"),
  bounds = c(-1, 9),
  est_len = 250
)

all_events <- all_events %>% set_names(c("Event1","Event2","Event3","Event4"))

# 2. Data Loading #####
# Get directories of files
print("Fetching data...")
start_time <- Sys.time()

cd <-
  'C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/daily/cleaned/es_format/'
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
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")

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
df2 <- subset(df2, rownames(df2) > as.Date("2019-01-29"))
df3 <- subset(df3, rownames(df3) > as.Date("2019-01-29"))

df2 <- subset(df2, rownames(df2) < as.Date("2020-06-01"))
df3 <- subset(df3, rownames(df3) < as.Date("2020-06-01"))

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
# have not tested if the above two lines can be removed without effects

end_time <- Sys.time()
paste("Complete. Time elapsed: ",
      round(end_time - start_time, digits = 4),
      "seconds")

# 4. Wrangling of Identification (strings) data ##################
cat(
  "Fetching variable identification data.",
  "\n",
  "Creating variables. Reformatting for r syntax..."
)
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

# 5. Creation of list of market-index data.frames ##############################

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
# 6. Creation of list of stock-index data.frames consisting of constituent-shares ####
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
      stock_series[rowSums(is.na(stock_series)) != ncol(stock_series), ]
    
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

# 7. Recording of problem data for inspection. STATUS = INACTIVE ########
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

# 8. Removal of remaining NAs #####
  # Process first drops rows where all observations are NAs (public holidays, etc.)
  # Process then drops remaining columns that still include AT LEAST 1 NA 
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
  selector <- selector[rowSums(is.na(selector)) != ncol(selector),]
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
  selector <- selector[rowSums(is.na(selector)) != ncol(selector),]
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

# 9. Pre-initialize objects for later loop ####
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

# Source "apply_market_model_gls.R"####
source("C:/Users/Keegan/Desktop/Repository/@ Development/estudy2_gls/R/apply_market_model_gls.R")
# Source development aid functions ####
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

# 10.Large loop that applies Estudy process over the large dataset for GEOGRAPHIC regions ####
for (i in 1:length(market_list)) {
  tryCatch({
    print(paste("Getting rates from prices for", keys[[i]]))
    rates <- estudy2::get_rates_from_prices(
      stock_list[[i]],
      quote = "Close",
      multi_day = TRUE,
      compounding = "continuous"
    )
    
    rates_indx <- estudy2::get_rates_from_prices(
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
    securities_returns <- apply_market_model.data.frame(
      rates = rates,
      regressor = rates_indx,
      same_regressor_for_all = TRUE,
      market_model = "sim",
      estimation_method = "gls",
      estimation_start = as.Date("2019-04-01"),
      estimation_end = as.Date("2020-03-13")
    )
    
    print("Done. Applying parametric and non-parametric tests to abnormal returns...")
    # ABNORMAL RETURN TESTS
    # Parametric tests
    ar_para <- data.frame(
      estudy2::parametric_tests(
        list_of_returns = securities_returns,
        event_start = as.Date("2020-03-16"),
        event_end = as.Date("2020-03-20")
      )
    )
    # Non-parametric tests
    ar_non_para <- data.frame(
      estudy2::nonparametric_tests(
        list_of_returns = securities_returns,
        event_start = as.Date("2020-03-16"),
        event_end = as.Date("2020-03-20")
      )
    )
    # CUMULATIVE ABNORMAL RETURN TESTS
    # Parametric tests
    car_para <- data.frame(
      estudy2::car_parametric_tests(
        list_of_returns = securities_returns,
        car_start = as.Date("2020-03-16"),
        car_end = as.Date("2020-03-20")
      )
    )
    # Non-parametric tests
    car_non_para <- data.frame(
      estudy2::car_nonparametric_tests(
        list_of_returns = securities_returns,
        car_start = as.Date("2020-03-16"),
        car_end = as.Date("2020-03-20")
      )
    )
    print(paste("Merging results.", keys[[i]]))
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

# 11. Recording of results in new '.csv' data-files #####
start_time <- Sys.time()

# WRITE RESULTS
cd_ar <-
  "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/geographic_region/ar_res/"
cd_car <-
  "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/geographic_region/car_res/"
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

# Records absolute directories for later
write.table(ar_results_filenames,
            file = paste0(cd_ar,"geo_ar_cd.txt"),
            quote = TRUE,
            row.names = FALSE,
            col.names = FALSE)
write.table(car_results_filenames,
            file = paste0(cd_car,"geo_car_cd.txt"),
            quote = TRUE,
            row.names = FALSE,
            col.names = FALSE)

end_time <- Sys.time()
paste("Complete. Time elapsed: ")
round(end_time - start_time, digits = 4)

# 12.1. Extract and store AR data from 'reg_results_list' ####
start_time <- Sys.time()
print("Extracting abnormal returns.")

# Extract and store ARs in list
ar_data <- reg_results_list 
for (i in 1:length(ar_data)) {
  tryCatch({
  indx <- keys[[i]]
  ticks <- names(stock_list[[indx]][-1]) # minus one to skip first row "date"
  names(ar_data[[indx]]) <- ticks
  
  ar_data_copy <- ar_data
  for (j in 1:length(ticks)) {
    tic <- ticks[[j]]
    ar_data[[indx]][[tic]] <- ar_data_copy[[indx]][[tic]]$abnormal
  }
  rm(ar_data_copy)
  }, error = function(e)
  {
    message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
  })
}

end_time <- Sys.time()
print("Extraction complete.")
round(end_time - start_time, digits = 4) %>%  print()

# 12.2 Reconfigure into lists of merged data.frame objects ####
start_time <- Sys.time()
# AR and CAR calculation
ar_data_merged <- ar_data
for (i in seq_along(ar_data)) {
  tryCatch({
    indx <- keys[[i]]
    ar_data_merged[[indx]] <- do.call(zoo::merge.zoo,
                                               ar_data[[indx]]
    ) %>% as.data.frame
  }, error = function(e)
  {
    message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
  })
}
# 12.3 Calculation of CARS ####
car_data_merged <- ar_data_merged
for (i in seq_along(ar_data_merged)) {
  tryCatch({
    indx <- keys[[i]]
    ticks <- names(ar_data_merged[[indx]])
    for (j in 1:ncol(ar_data_merged[[indx]])) {
      tryCatch({
        # Get name
        tic <- ticks[[j]]
        # Slice out series
        s1 <- ar_data_merged[[indx]][, tic]
        s2 <- s1
        # Accumulate returns, ignoring NAs
        s2[!is.na(s1)] <- cumsum(s2[!is.na(s1)])
        # store returns in order
        car_data_merged[[indx]][, tic] <- s2
        rm(s1, s2)
      }, error = function(e)
      {
        message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
      })
    }
    ar_data_merged[[indx]] <- as.data.frame(ar_data_merged[[indx]])
  }, error = function(e)
  {
    message(cat("ERROR: ", conditionMessage(e), "i = ", i, "\n"))
  })
}

# 12.3 Store ARs and CARs ####
d_base <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"
store_results(
  results = ar_data_merged,
  icb_level = "geographic_region",
  cd_root = d_base,
  return_type = "ar",
  type = "data",
  rowNames = TRUE
)
store_results(
  results = car_data_merged,
  icb_level = "geographic_region",
  cd_root = d_base,
  return_type = "car",
  type = "data",
  rowNames = TRUE
)
paste("Complete.")
round(end_time - start_time, digits = 4) %>%  print()

# 12.4. Calculate average abnormal returns ####
start_time <- Sys.time()
print("Calculating average ARs and CARS.")

aar_data_merged <- ar_data_merged
caar_data_merged <- car_data_merged
for (i in seq_along(ar_data)) {
  indx <- keys[[i]]
  aar_data_merged[[indx]] <- rowMeans(ar_data_merged[[indx]],
                                      na.rm = TRUE) %>% 
    as.data.frame() %>% 
    set_rownames(row.names(ar_data_merged[[indx]])) %>% 
    set_colnames(indx)
  caar_data_merged[[indx]] <- cumsum(aar_data_merged[[indx]])
}

# 12.5 Store AARs and CAARs  ####
store_results(
  results = aar_data_merged,
  icb_level = "geographic_region",
  cd_root = d_base,
  return_type = "aar",
  type = "data",
  rowNames = TRUE
)
store_results(
  results = caar_data_merged,
  icb_level = "geographic_region",
  cd_root = d_base,
  return_type = "caar",
  type = "data",
  rowNames = TRUE
)
paste("Complete.")
round(end_time - start_time, digits = 4) %>%  print()

print("Script complete.")
