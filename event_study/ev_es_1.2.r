print("Loading user-defined functions.")
start_time <- Sys.time()

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
paste("Complete. Time elapsed: ", end_time - start_time, "seconds")
################################################################################
# Data things
# Get directories of files
print("Fetching data...")
start_time <- Sys.time()

cd <-
  'C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/daily/cleaned/es_format/'
files <-
  c('stock_data_column-wise.csv',
    'market-index_data_column-wise.csv')

# Join the strings together --> actually was to practice loops as static would have been simpler in R
cds <- list()

for (i in files) {
  string <- paste(cd, i, sep = "")
  # print(string)
  cds <- append(cds, string,)
}

# Read in data
# No df1 because that would be the third file that contains everything in df2 and df3
df2 <- read.csv(as.character(cds[1]))
df3 <- read.csv(as.character(cds[2]))

# Remove duplicate index
df2 = subset(df2, select = -c(X))
df3 = subset(df3, select = -c(X))

# Rename 'Date' var into 'date'
names(df2)[names(df2) == 'Date'] <- 'date'
names(df3)[names(df3) == 'Date'] <- 'date'

end_time <- Sys.time()
paste("Complete. Time elapsed: ", end_time - start_time, "seconds")

print("Reformatting data.shape...")
start_time <- Sys.time()

# Reformatting 'date's into Date class
df2$date <- as.Date(df2$date, format = "%Y-%m-%d")
df3$date <- as.Date(df3$date, format = "%Y-%m-%d")

# Index by dates
rownames(df2) <- as.Date(df2$date, format = "%Y-%m-%d")
rownames(df3) <- as.Date(df3$date, format = "%Y-%m-%d")

# Need to fix the NaN problem of the data. See python notebook.

# Subset data.frames to get rid of the bulk of NaNs
df2 <- subset(df2, rownames(df2) > as.Date("2008-01-04"))
df3 <- subset(df3, rownames(df3) > as.Date("2008-01-04"))

# Store 'date' column
dates2 <- c(as.Date(df2$date))
dates3 <- c(as.Date(df3$date))

# Create new data.frames without 'date' cols
df2 <- subset(df2, select = -date)
df3 <- subset(df3, select = -date)

# Remove remaining rows where all is NaN
df2 <- df2[rowSums(is.na(df2)) != ncol(df2), ]
df3 <- df3[rowSums(is.na(df3)) != ncol(df3), ]

end_time <- Sys.time()
paste("Complete. Time elapsed: ", end_time - start_time, "seconds")

################################################################################

cat(
  "Fetching variable identification data.",
  "\n",
  "Creating variables. Reformatting for r syntax..."
)
start_time <- Sys.time()

cd_dict = "C:/Users/Keegan/iCloudDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/multi_series_data/id/index_member_dict_fixed.xlsx"
name_dict = readxl::read_xlsx(cd_dict)

# Split into list of things
df_3 <- as.list(df3)

# Get colnames
keys <- colnames(name_dict)

# Replace spaces with '.' to be able to select columns from data.frame
keys2 <- as.vector(gsub(" ", ".", keys))

# Initialise empty lists with required length
# indice_list <- vector(mode = "list", length = n)
stock_list <- vector(mode = "list", length = length(df_3))
market_list <- vector(mode = "list", length = length(df_3))

# Swap number index for names --> gives a python dictionary similarity
names(market_list) <- keys2
names(stock_list) <- keys2

# Break data.frame into list to remove NANs
name_list <- as.list(name_dict)
name_list <- lapply(name_list, na.omit)

pattern_list <- c(" ", "/", "-", "\\*", "&")

for (i in 1:length(pattern_list)) {
  name_list <-
    lapply(
      name_list,
      gsub,
      pattern = pattern_list[[i]],
      replacement = ".",
      perl = TRUE
    )
}

lapply(test_list_copy,stringr::str_replace_all,trouble_selector,NA_character_)

for (i in 1:length(trouble_selector)) {
  test_list <-
    lapply(
      test_list,
      stringr::str_replace_all,
      string = trouble_selector[[i]],
      replacement = NA_character_
    )
}

test_vec <- c("X009900.KS.Equity","ADMR.IJ.Equity","AEP.UW.Equity","X300866.CH.Equity","CTS.CT.Equity","ACWA.AB.Equity")
filler_vec <- c("adsjgfsjg","sdufhsdo","ahdsjfhask","sdjfsdlkfjs","asdafaf","asfsdfadfdf","asdfsdf")
test_list <- list(test_vec,filler_vec,filler_vec,filler_vec,test_vec,test_vec,filler_vec)
stringr::str_remove_all()
test_vec_copy <- test_vec
mini <- trouble_selector[1:3]
stringr::str_replace_all(test_vec_copy,trouble_selector[1],NA_character_)

# Less elegant solution
# If you're reading this, forgive my copy-paste.
# name_list <- lapply(name_list, gsub, pattern = " ", replacement = ".")
# name_list <- lapply(name_list, gsub, pattern = "/", replacement = ".")
# name_list <- lapply(name_list, gsub, pattern = "-", replacement = ".")
# name_list <- lapply(name_list, gsub, pattern = "\\*", replacement = ".")
# name_list <- lapply(name_list, gsub, pattern = "&", replacement = ".")

# Make syntactically compatible
names(name_list) <-
  lapply(names(name_list),
         gsub,
         pattern = " ",
         replacement = ".")
name_list <- lapply(name_list, fix_digit_names, "X")
aaa <- name_list
# If needed to make into a data.frame
# name_df <- as.data.frame(name_list)

end_time <- Sys.time()
paste("Complete. Time elapsed: ", end_time - start_time, "seconds")

################################################################################
# Vector of problematic names
print("Dropping problematic share names.")
start_time <- Sys.time()
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
    "STLA.FP.Equity", #NOTE: this is a double-up
    "ENR.GY.Equity",
    "HON.UW.Equity"
  )
trouble_selector <- unique(trouble)
trouble_selector <- paste("^",trouble_selector,"$", sep="")
trouble_selector <- paste0(trouble_selector, collapse = "|")

# Allocate colnames
names_to_keep <- names(df2)[!(names(df2) %in% trouble)]

# Carve off trouble names
for (i in 1:length(name_list)) {
  # plan
  # get index per vector-element per list element
  # use same index to remove vector-element per list element
  ilen <- 1:length(trouble_selector)
  selected_element <- name_list[[i]]
  for (j in ilen) {
    print(grep(trouble_selector[[j]], name_list[[i]]))
    temp_idx_iter <- grep(trouble_selector[[j]], name_list[[i]])
    if (temp_idx_iter == integer(0)) {
      next
    } else {
      idx <- grep(trouble_selector[[j]], name_list[[i]])
    }
    selected_element <- selected_element[-idx]
  }
  # name_list[[i]] <- selected_element
  # rm(selected_element)
}
vec <- c(integer(0),4,integer(0),integer(0),integer(0),1,3)
lissst <- list(vec,vec,integer(0),(2*vec), integer(0),(3*vec))
for (i in 1:length(lissst)) {
  if (lissst[[i]]==integer(0)) {
    next
  } else {
    print(lissst[i])
  }
}

df2_trouble <- subset(df2, select = trouble)
df2 <- subset(df2, select = names_to_keep)

for (i in 1:length(keys2)) {
  name_list[keys2[[i]]] <-
    name_list[keys2[[i]]][!apply(sapply(trouble, function(q) {
      grepl(q, name_list[keys2[[i]]])
    }), 1, function(x) {
      sum(as.numeric(x)) > 0
    })]
    # name_list[keys2[[i]]] [!name_list[keys2[[i]]] %in% grep(paste0(trouble, collapse = "|"),
  #                                                           name_list[keys2[[i]]],
  #                                                           value = T,
  #                                                           fixed = TRUE)]
  
}

x = c("a", "b", "c", "kt")
y = c("abs", "kot", "ccf", "okt", "kk", "y")
y[!y %in% grep(paste0(x, collapse = "|"), y, value = T, fixed = TRUE)]
y[!apply(sapply(x, function(q) {grepl(q, y)}), 1, function(x) {sum(as.numeric(x)) > 0})]

b <- name_list
# Check carve happened correctly
if ((any(trouble %in% names(df2)))==TRUE) {
  message("WARNING: Removal of problem columns failed")
} 

identical(aaa,b)

end_time <- Sys.time()
paste("Complete. Time elapsed: ", end_time - start_time, "seconds")

################################################################################

print("Making list of market data.frame")
start_time <- Sys.time()

for (i in 1:length(keys2)) {
  print(i)
  print(keys2[[i]])
  # Select as.data.frame the market index
  temp_df <- as.data.frame(df3[, keys2[[i]], drop = FALSE])
  temp_df <- cbind(date = dates2, temp_df)
  print(length(rownames(temp_df)))
  # rownames(temp_df) <- dates3
  
  market_list[[keys2[[i]]]] <- temp_df  
  #as.data.frame(temp_df, row.names = dates3_char) dates3_char <- as.character(dates3)
  
  rm(temp_df)
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", end_time - start_time, "seconds")

################################################################################

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
    
    # stores data in dictionary of index constituent pd.DataFrame --> Index name is key
    stock_list[[keys2[[i]]]] <- stock_series
  }, error = function(e) {
    message(cat("ERROR: ", conditionMessage(e), "\n"))
  })
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", end_time - start_time, "seconds")
################################################################################
cat(
  "Removing NANs for data.frames in lists:", "\n",
  "1. ", name_as_string(stock_list),"\n",
  "2. ", name_as_string(market_list)
)
start_time <- Sys.time()

# Make copies to check if results behaved as expected
market_list_copy <- market_list
stock_list_copy <- stock_list

for (i in 1:length(market_list)) {
  market_list[[i]] <- na.omit(market_list[[i]])
}
for (i in 1:length(stock_list)) {
  stock_list[[i]] <- na.omit(stock_list[[i]])
}

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
  message(cat("NANs have successfully been removed from the following lists", "\n",
              "1. ", name_as_string(market_list), "\n",
              "2. ", name_as_string(stock_list))
          )
}

end_time <- Sys.time()
paste("Complete. Time elapsed: ", end_time - start_time, "seconds")


################################################################################
library(estudy2)

# Create data storage lists
reg_results_list <-
  vector(mode = "list", length = length(market_list))
test_results_list <-
  vector(mode = "list", length = length(market_list))

# da big loop you've all been waiting for yessir
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