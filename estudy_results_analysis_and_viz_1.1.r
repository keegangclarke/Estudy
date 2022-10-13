# Load packages & functions
library(magrittr)
library(ggplot2)
library(bizdays)
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

# Directory components ####
d_root <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"
d_calenders <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/calendars/"

d_type <- c("geographic_region/","industry_classification/") %>% set_names(c("geo","icb"))
d_geo <- d_type[["geo"]]
d_icb <- d_type[["icb"]]

d_res_pres <- "results_presentation/"

d_id <- "id/"
d_supe <- "supersector/"
d_indu <- "industry/"

d_plot <- "plots/"
d_tables <- "tables/"

d_ar <- "ar/"
d_aar <- "aar/"
d_ar_res <- "ar_res/"

d_car <- "car/"
d_caar <- "caar/"
d_car_res <- "car_res/"

d_proto <- "prototypes/"


# LOAD ID INFO ####
# Static since variables not identical format
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

# 1. Parameters ####
# Get calendar infomation
rm_idx1 <- c("\\.Index")
cal_names <- paste0(d_root,
                    d_id,
                    d_geo,
                    "market_names.txt") %>%
  read.table(header = FALSE) %>%
  lapply(stringr::str_replace_all,
         pattern = rm_idx1,
         replacement = "") %>%
  lapply(as.factor) %>% 
  unlist() %>% 
  set_names(NULL) %>%
  casefold()

rm_idx2 <- c("\\.index")
temp_names <- geo_fac %>%
  casefold %>%
  lapply(stringr::str_replace_all,
         pattern = rm_idx2,
         replacement = "") %>% 
  unlist
# test if names are correct order
if(!identical(cal_names, temp_names)) {
  message("Calendar names do not match market names. Correct event specification is contingent on identical names and name order.")
} else(
  rm(temp_names, rm_idx2)
)
# add str identifiers for later
names(cal_names) <- geo_fac

d_cals <- make_list(Length = length(cal_names), cal_names)

for (d in seq_along(d_cals)) {
  print(d)
  cal <- cal_names[[d]]
  d_cals[[cal]] <- paste0(d_calenders,
                          cal, ".json")
  j.cal <- rjson::fromJSON(file=d_cals[[cal]])
  create.calendar(j.cal$name,
                  holidays = j.cal$holidays,
                  weekdays = j.cal$weekdays,
                  start.date = as.Date("2018-01-01"),
                  end.date = as.Date("2020-12-31"),
                  adjust.from = j.cal$adjust.from,
                  adjust.to = j.cal$adjust.to,
                  financial = j.cal$financial)
}
j.cal.uni <- rjson::fromJSON(file="C:/Users/Keegan/anaconda3/envs/ml/dev_files/mlstruct/calendars/no_holidays.json")
create.calendar(j.cal.uni$name,
                holidays = j.cal.uni$holidays,
                weekdays = j.cal.uni$weekdays,
                start.date = as.Date("2018-01-01"),
                end.date = as.Date("2020-12-31"),
                adjust.from = j.cal.uni$adjust.from,
                adjust.to = j.cal.uni$adjust.to,
                financial = j.cal.uni$financial)

# Function to get the right days
b_days <- function(Edate, wind_len, est_len, cal) {
  Edate <- as.Date(Edate)
  adj1 <- 1
  
  # event window
  lower <- bizdays::add.bizdays(Edate, wind_len[[1]], cal)
  upper <- bizdays::add.bizdays(Edate, wind_len[[2]], cal)
  bdays <- bizdays::bizseq(as.Date(lower), as.Date(upper), cal)
  
  # estimation widow
  est_end <- bizdays::add.bizdays(Edate, (wind_len[[1]]-adj1), cal)
  if (est_end >= lower) {
    while (est_end >= lower) {
      adj <- adj + 1
      est_end <-
        bizdays::add.bizdays(Edate, (wind_len[[1]] - adj1), cal)
    }
  }
  adj2 <- 1
  adj3 <- 1
  est_start <- bizdays::add.bizdays(Edate, (wind_len[[1]]-adj2-est_len), cal)
  est_bdays <- bizdays::bizseq(est_start, est_end, cal)
  
  # Logic ensures estimation days match requested estimation length
  est_start2 <- bizdays::add.bizdays(Edate,
                                     (wind_len[[1]] - adj2 - est_len),
                                     cal)
  est_start3 <- bizdays::add.bizdays(Edate,
                                     (wind_len[[1]] - adj3 - est_len),
                                     cal)
  est_bdays2 <- length(bizdays::bizseq(est_start2, est_end, cal))
  est_bdays3 <- length(bizdays::bizseq(est_start3, est_end, cal))
  if (length(est_bdays) != est_len) {
    while (all((est_bdays2 != est_len), (est_bdays3 != est_len))) {
      adj2 <- adj2 + 1
      adj3 <- adj3 - 1
      
      est_start2 <- bizdays::add.bizdays(Edate,
                                         (wind_len[[1]] - adj2 - est_len),
                                         cal)
      est_start3 <- bizdays::add.bizdays(Edate,
                                         (wind_len[[1]] - adj3 - est_len),
                                         cal)
      
      est_bdays2 <-
        length(bizdays::bizseq(est_start2, est_end, cal))
      est_bdays3 <-
        length(bizdays::bizseq(est_start3, est_end, cal))
    }
    if (est_bdays2 == est_len) {
      est_bdays <- bizdays::bizseq(est_start2, est_end, cal)
    } else if (est_bdays3 == est_len) {
      est_bdays <- bizdays::bizseq(est_start3, est_end, cal)
    }
  }
  # final check
  overlap <- as.Date(
    intersect(bdays,
              est_bdays),
    origin="1970-01-01")
  if (length(overlap)!=0) {
    message("There is an unexpected overlap between event and estimation windows. \n Please debug function logic.")
  }
  # package results and export
  bundle <- setNames(list(bdays, est_bdays),
                     c("event_window", "estimation_window"))
  return(bundle)
}

# Smaller version
b_days_around_date <- function(e_day, wind_len, cal) {
  e_day <- as.Date(e_day)
  
  lower <- bizdays::add.bizdays(e_day, wind_len[[1]], cal)
  upper <- bizdays::add.bizdays(e_day, wind_len[[2]], cal)
  
  bdays <- bizdays::bizseq(as.Date(lower), as.Date(upper), cal)
  
  return(bdays)
}

event_spec <- function(e_name = "",
                       grouping = "",
                       edate = NULL,
                       bounds = NULL,
                       est_len = NULL,
                       calendar =  NULL) {
  
  # Test for info
  if ((e_name == "") == TRUE) {
    message("Please specify event-name as string.")
  } else if (is.null(edate)) {
    message("Please specify event date.")
  } else if ((class(edate) != "Date") == TRUE) {
    message("Please ensure event date is 'as.Date'")
  } else if (is.null(bounds)) {
    message("Please specify event bounds.")
  } else if (is.null(est_len)) {
    message("Please specify estimation length.")
  } else if (is.null(calendar)) {
    message("Please specify a calender.")
  } else {
    # specify windows
    Windows <- b_days(Edate = edate,
                           wind_len = bounds,
                           est_len = est_len,
                           cal = calendar)
    event_window <- Windows$event_window
    estimation_window <- Windows$estimation_window
    
    event_specification <- vector(mode = 'list', length = 6)
    event_specification[[1]] <- grouping
    event_specification[[2]] <- e_name
    event_specification[[3]] <- edate
    event_specification[[4]] <- event_window
    event_specification[[5]] <- estimation_window
    event_specification[[6]] <- calendar
    
    event_specification <-
      setNames(
        event_specification,
        c(
          "group",
          "event_name",
          "event_date",
          "event_window",
          "estimation_window",
          "calendar"
        )
      )
    
    class(event_specification) <- "event_spec"
    return(event_specification)
  }
}

# list to store all event params
all_events <- make_list(4, 
                        c("event1", "event2", "event3", "event4"))
groupings <- make_list(Length = length(geo_fac),
                       geo_fac)

for (event in seq_along(all_events)) {
  all_events[[event]] <- groupings
}
for (geo in seq_along(geo_fac)) {
  group <- geo_fac[[geo]]
  CAL <- cal_names[[geo]]
  
  all_events[[1]][[group]] <- event_spec(
    e_name = "event1",
    grouping = group,
    edate = as.Date("2020-01-13"),
    bounds = c(-5, 5),
    est_len = 250,
    calendar = CAL
  )
  all_events[[2]][[group]] <- event_spec(
    e_name = "event2",
    grouping = group,
    edate = as.Date("2020-01-24"),
    bounds = c(-2, 8),
    est_len = 250,
    calendar = CAL
  )
  all_events[[3]][[group]] <- event_spec(
    e_name = "event3",
    grouping = group,
    edate = as.Date("2020-02-24"),
    bounds = c(-1, 9),
    est_len = 250,
    calendar = CAL
  )
  all_events[[4]][[group]] <- event_spec(
    e_name = "event4",
    grouping = group,
    edate = as.Date("2020-03-09"),
    bounds = c(-1, 9),
    est_len = 250,
    calendar = CAL
  )
}
# META PARAMS ####
e_meta <- make_list(4,c(c("event1", "event2", "event3", "event4")))
e_meta[[1]] <- event_spec(
  e_name = "event1",
  grouping = "meta",
  edate = as.Date("2020-01-13"),
  bounds = c(-5, 5),
  est_len = 250,
  calendar = "no_holidays"
)
e_meta[[2]] <- event_spec(
  e_name = "event2",
  grouping = "meta",
  edate = as.Date("2020-01-24"),
  bounds = c(-2, 8),
  est_len = 250,
  calendar = "no_holidays"
)
e_meta[[3]] <- event_spec(
  e_name = "event3",
  grouping = "meta",
  edate = as.Date("2020-02-24"),
  bounds = c(-1, 9),
  est_len = 250,
  calendar = "no_holidays"
)
e_meta[[4]] <- event_spec(
  e_name = "event4",
  grouping = "meta",
  edate = as.Date("2020-03-09"),
  bounds = c(-1, 9),
  est_len = 250,
  calendar = "no_holidays"
)

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
  storage_lst <- make_list(length(d), name_lst)
  # retrieve data
  storage_lst <- fetch_data(d,
                            lst = storage_lst,
                            file_type = "csv")
  # wrangle to be suitable
  for (i in seq_along(storage_lst)) {
    names(storage_lst[[i]])[[1]] <- "Date"
    storage_lst[[i]][["Date"]] <-
      storage_lst[[i]][["Date"]] %>% as.Date()
  }
  return(storage_lst)
}

# FUNCTION TO RETRIEVE STATISTICS ####
fetch_stats <- function(name_lst, directory) {
  d <- vector(mode = "character",
              length = length(name_lst))
  for (i in seq_along(d)) {
    d[[i]] <- paste0(directory,
                     name_lst[[i]],
                     ".csv")
  }
  # create storage list
  storage_lst <- make_list(length(d), name_lst)
  # retrieve data
  storage_lst <- fetch_data(d,
                            lst = storage_lst,
                            file_type = "csv")
  for (j in seq_along(storage_lst)) {
    # remove NAs
    storage_lst[[j]][is.na(storage_lst[[j]])] = ""
    storage_lst[[j]] <- subset(storage_lst[[j]], select=-c(X))
  }
  return(storage_lst)
}

# FUNCTION TO MERGE CAAR & AAR DATA LISTS TOGETHER FOR PLOTS ####
merge_caar_aar <-
  function(aar,
           caar,
           event_specification = NULL,
           alt_cal_days = NULL) {

        # "aar" & "caar" == data.frames of AARs and CAARs
    # "evt_day" == event_spec day by which 'Event.Time' will be calculated
    
    evt_day <- event_specification$event_date
    if (is.null(alt_cal_days)) {
    e_time_df <- setNames(as.data.frame(event_specification$event_window),
                          "Date")
    } else if (!is.null(alt_cal_days)) {
      e_time_df <- setNames(as.data.frame(alt_cal_days),
                            "Date")
    }
    # Is event day in event_window?
    zero <- evt_day %in% event_specification$event_window
    # calculates sequence of ints that represent the event-time
    e_time <- -sum(as.integer(e_time_df[["Date"]] < evt_day)):sum(as.integer(e_time_df[["Date"]] > evt_day))
    # neg <- -sum(as.integer(e_time_df[["Date"]] < evt_day)):0
    # pos <- 1:sum(as.integer(e_time_df[["Date"]] > evt_day))
    if (zero) {
      e_time_df$Event.Time <- e_time
      
    } else if (!zero) {
      e_time_df$Event.Time <- setdiff(e_time,0)
      # neg <- -sum(as.integer(e_time_df[["Date"]] < evt_day)):-1
      # if (any((length(neg) == 1) & (neg == 0))) {
      #   neg <- NULL
      #   e_time <- union(neg, pos)
      #   
      # } else {
      #   e_time <- union(neg, pos)
      #   
      # }
    }
    df <- merge.data.frame(aar,
                           caar,
                           by = "Date") %>%
      set_names(c('Date', "AAR", "CAAR"))
    df <- merge.data.frame(df, e_time_df, by = "Date")

    return(df)
  }


# FUNCTION TO PLOT AARs VS CAARs ####
aar_caar_plot <- function(name_lst, aar_caar_df_lst, PATH = NULL) {
  for (i in seq_along(name_lst)) {
    name <- name_lst[[i]]
    # AAR vs CAAR PLOT ####
    p_ave <-
      ggplot(data = aar_caar_df_lst[[name]]) +
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
      scale_x_continuous(breaks = aar_caar_df_lst[[name]]$Event.Time) +
      geom_hline(yintercept = 0,
                 size = 0.3) +
      geom_ribbon(aes(x = Event.Time,
                      ymin = AAR,
                      ymax = CAAR),
                  fill = "navyblue",
                  alpha = 0.1) +
      theme_bw() +
      labs(title = name,
           subtitle = "Average Abnormal Returns",
           y = "Abnormal Return",
           x = "Event day")
    
    # Store plot
    ggsave(
      p_ave,
      filename = paste0(name, "_aar_caar.png"),
      path = PATH,
      dpi = 320,
      width = 8,
      height = 4,
      units = 'in'
    )
  }
}

# USELESS FUNCTION TO SLICE LIST OF NAMES WITH INDICES IN REPEATED FASHION ####
slice_names <- function(name_list, Names) {
  lst <- vector(mode = "list", length = length(name_list))
  for (i in seq_along(Names)) {
    j <- which(scar_names == Names[[i]])
    lst[[j]] <- name_list[which(scar_names == Names[[i]])]
  }
  lst <- unlist(lst)
  return(lst)
}
# FUNCTION TO RECONFIGURE CARS SO THAT THEY ARE GROUPED BY STATISTIC ####
car_stats_tables <- function(car_list, name_vec) {
  df <- car_list[[1]]
  df[["group"]] <- names(car_list)[[1]]
  tmp_names <- name_vec[-1]
  
  for (i in seq_along(tmp_names)) {
    name <- tmp_names[[i]]
    df <- dplyr::full_join(df, car_list[[name]])
    df[is.na(df)] = as.character(name)
  }
  stats_fac <- as.factor(unique(df$name))
  store_lst <- make_list(length(stats_fac),
                         stats_fac)
  
  for (fac in stats_fac) {
    store_lst[[fac]] <- df %>%
      dplyr::filter(name == fac) %>%
      subset(select = -c(name))
  }
  return(store_lst)
}
#############
## META LOOP L1 ####
# Cycles through whole process for "geo" and "ICB"
for (type in 1:2) {
  TYPE <- d_type[[type]]
}

# META LOOP L2 ####
for (EVT in seq_along(e_meta)) {
  # SPECIFY PARAMS
  E_NAME <- e_meta[[EVT]]$event_name
  E_DAY <- e_meta[[EVT]]$event_date
  E_WIND <- e_meta[[EVT]]$event_window
  EST_WIND <- e_meta[[EVT]]$estimation_window
  
  E_DIR <- paste0(e_meta[[EVT]]$event_name, "/")

  print(E_NAME)
  # ARs & CARs: Retrieve data ####
  # GEOGRAPHIC
  ar_geo <- fetch_rtns(name_lst = geo_fac,
                       directory = paste0(d_root,
                                          d_geo,
                                          E_DIR,
                                          d_ar))
  # INDUSTRY
  ar_indu <- fetch_rtns(name_lst = indu_fac,
                        directory = paste0(d_root,
                                           d_icb,
                                           d_indu,
                                           E_DIR,
                                           d_ar))
  # SUPERSECTOR
  ar_supe <- fetch_rtns(name_lst = supe_fac,
                        directory = paste0(d_root,
                                           d_icb,
                                           d_supe,
                                           E_DIR,
                                           d_ar))
  
  # CUMULATIVE ABNORMAL RETURNS
  # GEOGRAPHIC
  car_geo <- fetch_rtns(name_lst = geo_fac,
                        directory = paste0(d_root,
                                           d_geo,
                                           E_DIR,
                                           d_car))
  # INDUSTRY
  car_indu <- fetch_rtns(name_lst = indu_fac,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_indu,
                                            E_DIR,
                                            d_car))
  # SUPERSECTOR
  car_supe <- fetch_rtns(name_lst = supe_fac,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_supe,
                                            E_DIR,
                                            d_car))
  
  # AARs: RETRIEVE ALL AAR DATA ####
  # GEOGRAPHIC
  aar_geo <- fetch_rtns(name_lst = geo_fac,
                        directory = paste0(d_root,
                                           d_geo,
                                           E_DIR,
                                           d_aar))
  # INDUSTRY
  aar_indu <- fetch_rtns(name_lst = indu_fac,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_indu,
                                            E_DIR,
                                            d_aar))
  # SUPERSECTOR
  aar_supe <- fetch_rtns(name_lst = supe_fac,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_supe,
                                            E_DIR,
                                            d_aar))
  
  # CAARs: RETRIEVE ALL CAAR DATA ####
  # GEOGRAPHIC
  caar_geo <- fetch_rtns(name_lst = geo_fac,
                         directory = paste0(d_root,
                                            d_geo,
                                            E_DIR,
                                            d_caar))
  # INDUSTRY
  caar_indu <- fetch_rtns(name_lst = indu_fac,
                          directory = paste0(d_root,
                                             d_icb,
                                             d_indu,
                                             E_DIR,
                                             d_caar))
  # SUPERSECTOR
  caar_supe <- fetch_rtns(name_lst = supe_fac,
                          directory = paste0(d_root,
                                             d_icb,
                                             d_supe,
                                             E_DIR,
                                             d_caar))
  
  
  # ARs & CARs: Retrieve STATS data ####
  # GEOGRAPHIC
  sar_geo <- fetch_stats(name_lst = geo_fac,
                       directory = paste0(d_root,
                                          d_geo,
                                          E_DIR,
                                          d_ar_res))
  # INDUSTRY
  sar_indu <- fetch_stats(name_lst = indu_fac,
                        directory = paste0(d_root,
                                           d_icb,
                                           d_indu,
                                           E_DIR,
                                           d_ar_res))
  # SUPERSECTOR
  sar_supe <- fetch_stats(name_lst = supe_fac,
                        directory = paste0(d_root,
                                           d_icb,
                                           d_supe,
                                           E_DIR,
                                           d_ar_res))
  sar_cols <- names(sar_geo[[1]])
  
  # CUMULATIVE ABNORMAL RETURNS
  # GEOGRAPHIC
  scar_geo <- fetch_stats(name_lst = geo_fac,
                        directory = paste0(d_root,
                                           d_geo,
                                           E_DIR,
                                           d_car_res)) %>% 
    car_stats_tables(geo_fac)
  # INDUSTRY
  scar_indu <- fetch_stats(name_lst = indu_fac,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_indu,
                                            E_DIR,
                                            d_car_res)) %>% 
    car_stats_tables(indu_fac)
  # SUPERSECTOR
  scar_supe <- fetch_stats(name_lst = supe_fac,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_supe,
                                            E_DIR,
                                            d_car_res)) %>% 
    car_stats_tables(supe_fac)
  
  scar_cols <- names(scar_geo[[1]])
  # Merge AAR & CAAR data.frames ####
  # GEOGRAPHIC
  ave_lst_geo <- make_list(length(geo_fac), geo_fac)
  for (i in seq_along(ave_lst_geo)) {
    EVENT_SPEC <- all_events[[E_NAME]][[i]]
    name <- EVENT_SPEC$group
    
    temp_df <- merge_caar_aar(aar = aar_geo[[name]],
                              caar = caar_geo[[name]],
                              event_specification = EVENT_SPEC)
    ave_lst_geo[[name]] <- temp_df
    rm(temp_df)
  }
  # INDUSTRY
  ave_lst_indu <- make_list(length(indu_fac), indu_fac)
  for (i in seq_along(ave_lst_indu)) {
    name <- indu_fac[[i]]
    
    temp_df <- merge_caar_aar(aar = aar_indu[[name]],
                              caar = caar_indu[[name]],
                              event_specification = e_meta[[EVT]])
    ave_lst_indu[[name]] <- temp_df
    rm(temp_df)
  }
  # SUPERSECTOR
  ave_lst_supe <- make_list(length(supe_fac), supe_fac)
  for (i in seq_along(ave_lst_supe)) {
    name <- supe_fac[[i]]
    
    temp_df <- merge_caar_aar(aar = aar_supe[[name]],
                              caar = caar_supe[[name]],
                              event_specification = e_meta[[EVT]])
    ave_lst_supe[[name]] <- temp_df
    rm(temp_df)
  }
  
  # PLOT AARs V CAARs ####
  # GEOGRAPHIC
  aar_caar_plot(
    name_lst = geo_fac,
    aar_caar_df_lst = ave_lst_geo,
    PATH = paste0(d_root,
                  d_res_pres,
                  d_plot,
                  E_DIR,
                  d_geo,
                  "aar_caar/")
  )
  # INDUSTRY
  aar_caar_plot(
    name_lst = indu_fac,
    aar_caar_df_lst = ave_lst_indu,
    PATH = paste0(d_root,
                  d_res_pres,
                  d_plot,
                  E_DIR,
                  d_icb,
                  d_indu,
                  "aar_caar/")
  )
  # SUPERSECTOR
  aar_caar_plot(
    name_lst = indu_fac,
    aar_caar_df_lst = ave_lst_indu,
    PATH = paste0(d_root,
                  d_res_pres,
                  d_plot,
                  E_DIR,
                  d_icb,
                  d_supe,
                  "aar_caar/")
  )
  
  # BUILD TABLES ####
  
  scar_indu <- car_stats_tables(scar_indu, indu_fac)
  
  
}

# PLOT ARs ####
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
    geom_hline(yintercept = 0,
               size = 0.3) +
    geom_ribbon(aes(x = Event.Time,
                    ymin = AAR,
                    ymax = CAAR),
                fill = "navyblue",
                alpha = 0.1) +
    scale_x_continuous(breaks = ave_lst_geo[[indx]]$Event.Time) +
    theme_bw() +
    labs(title = indx,
         subtitle = "Average Abnormal Returns",
         y = "Abnormal Return (%)",
         x = "Event day")
  
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
p1 <-
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
p2 <-
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
