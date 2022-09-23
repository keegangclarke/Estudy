# Load packages & functions
library(magrittr)
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

# 0. Load basic components ####
# Parameters
event_day <- as.Date("2020-01-22")

# Directory components
d_root <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"
d_geo <- "geographic_region/"
d_icb <- "industry_classification/"
d_res_pres <- "results_presentation/"
d_id <- "id/"

d_plot <- "plots/"
d_tables <- "tables/"

d_ar <- "ar/"
d_aar <- "aar/"
d_car <- "car/"
d_caar <- "caar/"

d_proto <- "prototypes/"

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

# Specify patterns
pattern_list <- c(" ", "/", "-", "\\*", "&")
# Load regional data
df_region <- readxl::read_xlsx(paste0(d_root,
                                      d_res_pres,
                                      d_tables,
                                      "table_2_data.xlsx")) %>%
  lapply(stringr::str_replace_all,
         pattern = paste0(pattern_list,
         collapse = "|"), replacement = ".") %>%
  lapply(as.factor) %>% 
  as.data.frame()

# Load test files
# PERIODIC ABNORMAL RETURNS
ar_test_file <- read.csv(file = paste0(d_root,
                                       d_geo,
                                       d_ar,
                                       market_names[[1]],
                                       ".csv")) %>%
  as.data.frame()
colnames(ar_test_file)[[1]] <- 'date'
ar_test_file[['date']] <- as.Date(ar_test_file[['date']])

# CUMULATIVE ABNORMAL RETURNS
car_test_file <- read.csv(file = paste0(d_root,
                                        d_geo,
                                        d_car,
                                        market_names[[1]],
                                        ".csv")) %>%
  as.data.frame()
colnames(car_test_file)[[1]] <- 'date'
car_test_file[['date']] <- as.Date(car_test_file[['date']])


# Read in some identifying information
regions <- readxl::read_xlsx(
  path = paste0(d_root,
                d_res_pres,
                "table_region_classification_simplified.xlsx")) %>% 
  as.data.frame()

# AARs: RETRIEVE ALL AAR DATA ####
# create list of directories
aar_d_geo <- vector(mode = "character",
                    length = length(market_names))
for (i in seq_along(aar_d_geo)) {
  aar_d_geo[[i]] <- paste0(d_root,
                           d_geo,
                           d_caar,
                           market_names[[i]],
                           ".csv")
}
# create storage list
aars_geo <- vector(mode = "list",
                    length(aar_d_geo))
# wrangle to be suitable
aars_geo <- fetch_data(aar_d_geo,
                       lst = aars_geo,
                       file_type = "csv") %>%
  setNames(market_names)

for (i in seq_along(aars_geo)) {
  names(aars_geo[[i]])[[1]] <- "Date"
  aars_geo[[i]][["Date"]] <- aars_geo[[i]][["Date"]] %>% as.Date()
}

# CAARs: RETRIEVE ALL CAAR DATA ####
# create list of directories
caar_d_geo <- vector(mode = "character",
                     length = length(market_names))
for (i in seq_along(caar_d_geo)) {
  caar_d_geo[[i]] <- paste0(d_root,
                            d_geo,
                            d_caar,
                            market_names[[i]],
                            ".csv")
}
# create storage list
caars_geo <- vector(mode = "list",
                    length(caar_d_geo))
# retrieve data
caars_geo <- fetch_data(caar_d_geo,
                        lst = caars_geo,
                        file_type = "csv") %>%
  setNames(market_names)
# wrangle to be suitable
for (i in seq_along(caars_geo)) {
  names(caars_geo[[i]])[[1]] <- "Date"
  caars_geo[[i]][["Date"]] <- caars_geo[[i]][["Date"]] %>% as.Date()
}

# Merge AAR & CAAR data.frames, 
ave_lst <-
  vector(mode = "list", length(market_names)) %>% set_names(market_names)
for (i in seq_along(market_names)) {
  indx <- market_names[[i]]
  
  ave_lst[[i]] <- merge.data.frame(aars_geo[[indx]],
                                   caars_geo[[indx]],
                                   by = "Date") %>%
    set_names(c('Date', "AAR", "CAAR"))
  # adds event time column but calculates on logical basis an int vector which sets 0 to 'event_day'
  # negative ints for days before 'event_day'
  # postive ints for days after 'event_day'
  ave_lst[[indx]][["Event.Time"]] <-
    -nrow(aars_geo[[indx]][(aars_geo[[indx]][["Date"]] < event_day),]):nrow(aars_geo[[indx]][(aars_geo[[indx]][["Date"]] > event_day),])
  
}

library(ggplot2)

# PROTOTYPE AAR CAAR PLOT ####
ave_frame <- merge.data.frame(aars_geo[["KOSPI.Index"]],
                              caars_geo[["KOSPI.Index"]],
                              by = "date") %>%
  set_names(c('Date', "AAR", "CAAR"))

p_ave <- ggplot(data = ave_frame) +
  geom_line(mapping = aes(x = Date,
                          y = AAR),
            colour = "navyblue",
            show.legend = TRUE) +
  geom_line(mapping = aes(x = Date,
                          y = CAAR),
            show.legend = TRUE,
            colour = "darkred") +
  geom_hline(yintercept = 0,
             size = 0.3)+
  geom_ribbon(aes(x = Date,
                  ymin=AAR,
                  ymax=CAAR),
              fill="navyblue", 
              alpha=0.1)+
  theme_bw() +
  labs(title = paste("Average Abnormal Returns:",
                     names(aar_test_file)[2]),
  )

# Store plot
ggsave(
    p_ave,
    filename = 'prototype_aar_caar.png',
    path = paste0(d_root,
                  d_res_pres,
                  d_plot,
                  d_proto),
    dpi = 300,
    width = 8,
    height = 4,
    units = 'in'
  )
