# Load packages
library(magrittr)

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
cd_root <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"
cd_geo <- "geographic_region/"
cd_icb <- "industry_classication/"
cd_ar <- "ar/"
cd_car <- "car/"
# String-name info
cd_market_names <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/geographic_region/names.csv"

market_names <- cd_market_names %>% 
  read.csv(header = TRUE) %>% 
  unlist %>% 
  as.vector(mode = "character")

# Use paste0 to get the right names

# Load 2 files for testing
ar_test_file <- read.csv(file = paste0(cd_root,
                                       cd_geo,
                                       cd_ar,
                                       market_names[[1]],
                                       ".csv")) %>% 
  subset(select = -c(X))
car_test_file <- read.csv(file = paste0(cd_root,
                                       cd_geo,
                                       cd_car,
                                       market_names[[1]],
                                       ".csv")) %>% 
  subset(select = -c(X))
