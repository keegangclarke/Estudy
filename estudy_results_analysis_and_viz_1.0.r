# Load packages & functions
library(magrittr)
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

# 0. Specify directory strings and load string info ####
# Directory components
d_root <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"
d_geo <- "geographic_region/"
d_icb <- "industry_classification/"
d_ar <- "ar/"
d_aar <- "aar/"
d_car <- "car/"
d_caar <- "caar/"
d_id <- "id/"
d_res_pres <- "results_presentation/"

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

aar_test_file <- read.csv(file = paste0(d_root,
                                        d_geo,
                                        d_aar,
                                        market_names[[1]],
                                        ".csv")) %>%
  as.data.frame()
colnames(aar_test_file)[[1]] <- 'date'
aar_test_file[['date']] <- as.Date(aar_test_file[['date']])

# CUMULATIVE ABNORMAL RETURNS
car_test_file <- read.csv(file = paste0(d_root,
                                        d_geo,
                                        d_car,
                                        market_names[[1]],
                                        ".csv")) %>%
  as.data.frame()
colnames(car_test_file)[[1]] <- 'date'
car_test_file[['date']] <- as.Date(car_test_file[['date']])

caar_test_file <- read.csv(file = paste0(d_root,
                                         d_geo,
                                         d_caar,
                                         market_names[[1]],
                                         ".csv")) %>%
  as.data.frame()
colnames(caar_test_file)[[1]] <- 'date'
caar_test_file[['date']] <- as.Date(caar_test_file[['date']])

# Read in some identifying information
regions <- readxl::read_xlsx(path = paste0(d_root,
                                           d_res_pres,
                                           "table_region_classification_simplified.xlsx")) %>% 
  as.data.frame()

library(ggplot2)

ggplot() +
  geom_line(data = aar_test_file,
            mapping = aes(x = date,
                          y = KOSPI.Index,
                          size = 0.1),
            colour = "black",
            show.legend = TRUE) +
  # geom_smooth(formula = y~x,
  #             data = aar_test_file,
  #             show.legend = TRUE) +
  geom_line(data = caar_test_file,
            mapping = aes(x = date,
                          y = KOSPI.Index),
            show.legend = TRUE,
            colour = "blue") +
  geom_ribbon(#data=subset(x, 2 <= x & x <= 3),
              aes(ymin=aar_test_file$KOSPI.Index,
                  ymax=caar_test_file$KOSPI.Index),
              fill="blue", alpha=0.1)
  theme_bw() +
  labs(title = paste("Average Abnormal Returns:",
                     names(aar_test_file)[2]),
       )

a_frame <- merge.data.frame(aar_test_file,
                            caar_test_file,
                            by = "date") %>%
  set_names(c('Date', "AAR", "CAAR"))
p2 <- ggplot(a_frame) +
  geom_line(mapping = aes(x = Date,
                          y = AAR, color = AAR),
            show.legend = TRUE) +
  geom_line(a_frame, mapping = aes(x = Date,
                                   y = CAAR))

p3 <- ggplot(a_frame,
             aes(x = Date, y = AAR))+
  geom_point(pch = 21, size = log((a_frame$AAR)^2), fill = I("darkorchid1"))


p4 <- ggplot(a_frame,
             aes(x = Date, y = AAR))+
  geom_point()

caar_d_geo <- vector(mode = "character",
                           length = length(market_names))
for (i in seq_along(caar_d_geo)) {
  caar_d_geo[[i]] <- paste0(d_root,
                            d_geo,
                            d_caar,
                            market_names[[i]],
                            ".csv")
}
caars_geo <- vector(mode = "list",
                    length(caar_d_geo))

caars_geo <- fetch_data(caar_d_geo, lst = caars_geo, file_type = "csv") %>%
  setNames(market_names)

for (i in seq_along(caars_geo)) {
  names(caars_geo[[i]])[[1]] <- "Date"
}


p5 <- ggplot(data = a_frame) +
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
  geom_ribbon(#data=subset(x, 2 <= x & x <= 3),
    aes(x = Date,
        ymin=AAR,
        ymax=CAAR),
    fill="navyblue", alpha=0.1)
  theme_bw() +
  labs(title = paste("Average Abnormal Returns:",
                     names(aar_test_file)[2]),
  )

ggsave(p5, filename = 'nhtemp_with_cairo.png', dpi = 300, type = 'cairo',
         width = 8, height = 4, units = 'in')
