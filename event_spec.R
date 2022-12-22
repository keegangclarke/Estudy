# EVENT SPEC
library(magrittr)
library(bizdays)
# data directories
d_root <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"
d_calenders <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/calendars/"

d_type <- c("geographic_region/","industry_classification/") %>% set_names(c("geo","icb"))
d_geo <- d_type[["geo"]]
d_icb <- d_type[["icb"]]

d_res_pres <- "results_presentation/"

d_id <- "id/"
d_supe <- "supersector/"
d_indu <- "industry/"

geo_fac <- paste0(d_root,
                  d_id,
                  d_geo,
                  "market_names.txt") %>%
  read.table(header = FALSE) %>%
  unlist %>%
  as.vector(mode = "character")

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
# j.cal.uni <- rjson::fromJSON(file="C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/calendars/universal.json")
# create.calendar(j.cal.uni$name,
#                 holidays = j.cal.uni$holidays,
#                 weekdays = j.cal.uni$weekdays,
#                 start.date = as.Date("2018-01-01"),
#                 end.date = as.Date("2020-12-31"),
#                 adjust.from = j.cal.uni$adjust.from,
#                 adjust.to = j.cal.uni$adjust.to,
#                 financial = j.cal.uni$financial)
j.cal.uni <- rjson::fromJSON(file="C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/calendars/universal2.json")
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
  
  elen <- abs(wind_len[[1]]) + abs(wind_len[[2]])+1
  alen <- length(bdays)
  
  if (elen < alen) {
    upper <- bizdays::add.bizdays(Edate, wind_len[[2]]-1, cal)
    bdays <- bizdays::bizseq(as.Date(lower), as.Date(upper), cal)
  } else if (elen > alen) {
    upper <- bizdays::add.bizdays(Edate, wind_len[[2]]+1, cal)
    bdays <- bizdays::bizseq(as.Date(lower), as.Date(upper), cal)
  }
  
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
