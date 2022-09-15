# Load packages
library(magrittr)
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

# CHECK TYPES OF OBJECT
check_types <- function(object) {
  cat("typeof:", typeof(object), "\n")
  cat("class:", class(object), "\n")
  cat("mode:", mode(object), "\n")
  cat("attributes:", "\n") 
  print(attributes(object))
} 

# 0. Specify directory strings and load string info####
# Directory components
d_root <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"
d_geo <- "geographic_region/"
d_icb <- "industry_classification/"
d_ar <- "ar/"
d_car <- "car/"
d_id <- "id/"

# Load id info
market_names <- paste0(d_root,
                       d_id,
                       d_geo,
                       "market_names.txt") %>% 
  read.table(header = FALSE) %>% 
  unlist %>%
  as.vector(mode = "character")

df_sector_id <- paste0(d_root,
                       d_id,
                       d_icb,
                       "sector_data.csv") %>%
  read.csv(header = TRUE)

# Use paste0 to get the right names

# Load 2 files for testing
ar_test_file <- read.table(file = paste0(d_root,
                                       d_geo,
                                       d_ar,
                                       market_names[[1]],
                                       ".csv")) %>% 
  subset(select = -c(X))
car_test_file <- read.csv(file = paste0(d_root,
                                       d_geo,
                                       d_car,
                                       market_names[[1]],
                                       ".csv")) %>% 
  subset(select = -c(X))
