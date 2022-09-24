# Load packages & functions
library(magrittr)
library(ggplot2)
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

# Parameters ####
event_day <- as.Date("2020-01-22")

# Directory components ####
d_root <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"
d_geo <- "geographic_region/"
d_icb <- "industry_classification/"
d_res_pres <- "results_presentation/"

d_id <- "id/"
d_supe <- "supersector/"
d_indu <- "industry/"

d_plot <- "plots/"
d_tables <- "tables/"

d_ar <- "ar/"
d_aar <- "aar/"
d_car <- "car/"
d_caar <- "caar/"

d_proto <- "prototypes/"

# Load id info ####
geo_fac <- paste0(d_root,
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
# Create industry ID variables
industry <- df_sector_id[,1:2] %>%
  as.data.frame
indu_fac <- industry[,2] %>%
  unique %>%
  as.factor
# Create supersector ID variables
supersector <- cbind.data.frame(df_sector_id[,1], df_sector_id[,3]) %>% 
  set_colnames(c(names(df_sector_id)[[1]],
                 names(df_sector_id)[[3]]))
supe_fac <- supersector[,2] %>%
  unique %>%
  as.factor

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

# FUNCTION TO GET ABNORMAL RETURN DATA ####
fetch_rtns <- function(name_lst, directory) {
  d <- vector(mode = "character",
              length = length(name_lst))
  for (i in seq_along(d)) {
    d[[i]] <- paste0(directory,
                     name_lst[[i]],
                     ".csv")
  }
  # create storage list
  storage_lst <- vector(mode = "list",
                        length(d))
  # retrieve data
  storage_lst <- fetch_data(d,
                            lst = storage_lst,
                            file_type = "csv") %>%
    setNames(name_lst)
  # wrangle to be suitable
  for (i in seq_along(storage_lst)) {
    names(storage_lst[[i]])[[1]] <- "Date"
    storage_lst[[i]][["Date"]] <-
      storage_lst[[i]][["Date"]] %>% as.Date()
  }
  return(storage_lst)
}
# ARs & CARs: Retrieve data ####
# GEOGRAPHIC
ar_geo <- fetch_ars(name_lst = geo_fac,
                    directory = paste0(d_root,
                                       d_geo,
                                       d_ar))
# INDUSTRY
ar_indu <- fetch_ars(name_lst = indu_fac,
                    directory = paste0(d_root,
                                       d_icb,
                                       d_indu,
                                       d_ar))
# SUPERSECTOR
ar_supe <- fetch_ars(name_lst = supe_fac,
                    directory = paste0(d_root,
                                       d_icb,
                                       d_supe,
                                       d_ar))

# CUMULATIVE ABNORMAL RETURNS
# GEOGRAPHIC
car_geo <- fetch_ars(name_lst = geo_fac,
                    directory = paste0(d_root,
                                       d_geo,
                                       d_car))
# INDUSTRY
car_indu <- fetch_ars(name_lst = indu_fac,
                     directory = paste0(d_root,
                                        d_icb,
                                        d_indu,
                                        d_car))
# SUPERSECTOR
car_supe <- fetch_ars(name_lst = supe_fac,
                     directory = paste0(d_root,
                                        d_icb,
                                        d_supe,
                                        d_car))

# AARs: RETRIEVE ALL AAR DATA ####
# GEOGRAPHIC
aar_geo <- fetch_rtns(name_lst = geo_fac,
                         directory = paste0(d_root,
                                            d_geo,
                                            d_aar))
# INDUSTRY
aar_indu <- fetch_rtns(name_lst = indu_fac,
                          directory = paste0(d_root,
                                             d_icb,
                                             d_indu,
                                             d_aar))
# SUPERSECTOR
aar_supe <- fetch_rtns(name_lst = supe_fac,
                          directory = paste0(d_root,
                                             d_icb,
                                             d_supe,
                                             d_aar))

# CAARs: RETRIEVE ALL CAAR DATA ####
# GEOGRAPHIC
caar_geo <- fetch_rtns(name_lst = geo_fac,
                        directory = paste0(d_root,
                                           d_geo,
                                           d_caar))
# INDUSTRY
caar_indu <- fetch_rtns(name_lst = indu_fac,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_indu,
                                            d_caar))
# SUPERSECTOR
caar_supe <- fetch_rtns(name_lst = supe_fac,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_supe,
                                            d_caar))

# FUNCTION TO MERGE CAAR & AAR DATA LISTS TOGETHER FOR PLOTS ####
merge_caar_aar <-
  function(storage_lst,
           name_lst,
           aar_lst,
           caar_lst,
           evt_day = event_day) {
    # "storage_lst" == empty list of correct length to store merged data.frames
    # "name_lst" == list of names of groupings
    # "aar_lst" & "caar_lst" == lists of AARs and CAARs
    # "evt_day" == event day by which 'Event.Time' will be calculated
    for (i in seq_along(name_lst)) {
      nam <- name_lst[[i]]
      
      storage_lst[[i]] <- merge.data.frame(aar_lst[[nam]],
                                           caar_lst[[nam]],
                                           by = "Date") %>%
        set_names(c('Date', "AAR", "CAAR"))
      # adds event time column but calculates on logical basis an int vector which sets 0 to 'event_day'
      # negative ints for days before 'event_day'
      # postive ints for days after 'event_day'
      storage_lst[[nam]][["Event.Time"]] <-
        -nrow(aar_lst[[nam]][(aar_lst[[nam]][["Date"]] < evt_day),]):nrow(aar_lst[[nam]][(aar_lst[[nam]][["Date"]] > evt_day),])
    }
    return(storage_lst)
  }
# Merge AAR & CAAR data.frames ####
# GEOGRAPHIC
ave_lst_geo <-
  vector(mode = "list", length(geo_fac)) %>% set_names(geo_fac)
ave_lst_geo <- merge_caar_aar(storage_lst = ave_lst_geo,
                               name_lst = geo_fac,
                               aar_lst = aar_geo,
                               caar_lst = caar_geo)
# INDUSTRY
ave_lst_indu <- vector(mode = "list",
                       length(indu_fac)) %>%
  set_names(indu_fac)
ave_lst_indu <- merge_caar_aar(storage_lst = ave_lst_indu,
                               name_lst = indu_fac,
                               aar_lst = aar_indu,
                               caar_lst = caar_indu)
# SUPERSECTOR
ave_lst_supe <- vector(mode = "list",
                       length(supe_fac)) %>%
  set_names(supe_fac)
ave_lst_supe <- merge_caar_aar(storage_lst = ave_lst_supe,
                               name_lst = supe_fac,
                               aar_lst = aar_supe,
                               caar_lst = caar_supe)


# PLOT AARs V CAARs ####
for (i in seq_along(geo_fac)) {
  indx <- geo_fac[[i]]
  # AAR vs CAAR PLOT ####
  p_ave <-
    ggplot(data = ave_lst_geo[[indx]]) +
    geom_line(
      mapping = aes(x = Event.Time,
                    y = AAR),
      colour = "navyblue",
      show.legend = TRUE
    ) +
    geom_line(
      mapping = aes(x = Event.Time,
                    y = CAAR),
      colour = "darkred",
      show.legend = TRUE
    ) +
    scale_x_continuous(breaks = round(seq(min(ave_lst_geo[[indx]]$Event.Time),
                                          max(ave_lst_geo[[indx]]$Event.Time),
                                          by = 10),
                                      1)) +
    scale_y_continuous(breaks = round(seq(min(ave_lst_geo[[indx]]$Event.Time),
                                          max(ave_lst_geo[[indx]]$Event.Time),
                                          by = 10),
                                      1)) +
    geom_hline(yintercept = 0,
               size = 0.3) +
    geom_ribbon(aes(x = Event.Time,
                    ymin = AAR,
                    ymax = CAAR),
                fill = "navyblue",
                alpha = 0.1) +
    theme_bw() +
    labs(title = paste("Average Abnormal Returns:",
                       indx),)
  
  # Store plot
  ggsave(
    p_ave,
    filename = paste0(indx, "_aar_caar.png"),
    path = paste0(d_root,
                  d_res_pres,
                  d_plot,
                  d_geo,
                  "aar_caar/"),
    dpi = 320,
    width = 8,
    height = 4,
    units = 'in'
  )
}

# FOR EXPERIMENTS ####
indx <- geo_fac[[i]]
# AAR vs CAAR PLOT ####
p_ave <-
  ggplot(data = ave_lst_geo[[indx]]) +
  geom_line(
    mapping = aes(x = Event.Time,
                  y = AAR),
    colour = "navyblue",
    show.legend = TRUE
  ) +
  geom_line(
    mapping = aes(x = Event.Time,
                  y = CAAR),
    colour = "darkred",
    show.legend = TRUE
  ) +
  scale_x_continuous(breaks = round(seq(min(ave_lst_geo[[indx]]$Event.Time),
                                        max(ave_lst_geo[[indx]]$Event.Time),
                                        by = 10),
                                    1)) +
  scale_y_continuous(breaks = round(seq(min(ave_lst_geo[[indx]]$Event.Time),
                                        max(ave_lst_geo[[indx]]$Event.Time),
                                        by = 10),
                                    1)) +
  geom_hline(yintercept = 0,
             size = 0.3) +
  geom_ribbon(aes(x = Event.Time,
                  ymin = AAR,
                  ymax = CAAR),
              fill = "navyblue",
              alpha = 0.1) +
  theme_bw() +
  labs(title = paste("Average Abnormal Returns:",
                     indx),)
