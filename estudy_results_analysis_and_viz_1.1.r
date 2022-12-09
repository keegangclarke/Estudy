# Load packages & functions
library(magrittr)
library(ggplot2)
library(bizdays)
library(scales) # use for labels
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
geo_names <- paste0(d_root,
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
indu_names <- industry[,2] %>%
  unique %>%
  as.character
# Create supersector ID variables
supersector <- cbind.data.frame(df_sector_id[,1], df_sector_id[,3]) %>% 
  set_colnames(c(names(df_sector_id)[[1]],
                 names(df_sector_id)[[3]]))
supe_names <- supersector[,2] %>%
  unique %>%
  as.character

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
temp_names <- geo_names %>%
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
names(cal_names) <- geo_names

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
groupings <- make_list(Length = length(geo_names),
                       geo_names)

for (event in seq_along(all_events)) {
  all_events[[event]] <- groupings
}
for (geo in seq_along(geo_names)) {
  group <- geo_names[[geo]]
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

# F: fetch_rtns() # GET ABNORMAL RETURN DATA ####
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

# F: fetch_stats() # RETRIEVE STATISTICS ####
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

# F: merge_caar_aar() # MERGE CAAR & AAR DATA LISTS TOGETHER FOR PLOTS ####
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


# F: aar_caar_plot() # PLOT AARs VS CAARs ####
aar_caar_plot <- function(name_lst, aar_caar_df_lst, SCALE = 100, GROUP=NULL, PATH = NULL) {
  for (i in seq_along(name_lst)) {
    name <- name_lst[[i]]
    # AAR vs CAAR PLOT ####
    p_ave <-
      ggplot(data = aar_caar_df_lst[[name]]) +
      geom_line(
        mapping = aes(x = Event.Time,
                      y = AAR*SCALE,
                      colour = "AAR"),
        # label = 'Average Abnormal Return',
        show.legend = TRUE
      ) + labs(color = 'AAR')+
      geom_line(
        mapping = aes(x = Event.Time,
                      y = CAAR*SCALE,
                      colour = "CAAR"),
        # label = 'Cumulative Average Abnormal Return',
        show.legend = TRUE
      ) +
      scale_x_continuous(breaks = aar_caar_df_lst[[name]]$Event.Time) +
      geom_hline(yintercept = 0,
                 size = 0.3) +
      geom_ribbon(aes(x = Event.Time,
                      ymin = AAR*SCALE,
                      ymax = CAAR*SCALE),
                  fill = "navyblue",
                  alpha = 0.1) +
      theme_bw() +
        scale_color_manual(name = "Type",
                           values = c("AAR" = "navyblue",
                                      "CAAR" = "darkred")) +
      labs(title = paste0(GROUP, ': ', name),
           subtitle = "Average Abnormal Returns",
           y = "Abnormal Return (%)",
           x = "Event day")
    
    # Store plot
    ggsave(
      p_ave,
      filename = paste0(name, "_aar_caar.png"),
      path = PATH,
      dpi = 600,
      width = 8,
      height = 4,
      units = 'in'
    )
  }
}

# USELESS F: slice_names() # SLICE LIST OF NAMES WITH INDICES IN REPEATED FASHION ####
slice_names <- function(name_list, Names) {
  lst <- vector(mode = "list", length = length(name_list))
  for (i in seq_along(Names)) {
    j <- which(scar_names == Names[[i]])
    lst[[j]] <- name_list[which(scar_names == Names[[i]])]
  }
  lst <- unlist(lst)
  return(lst)
}
# F: car_stats_tables() # RECONFIGURE CARS SO THAT THEY ARE GROUPED BY STATISTIC ####
car_stats_tables <- function(car_list, name_vec) {
  df <- car_list[[1]]
  df[["group"]] <- names(car_list)[[1]]
  tmp_names <- name_vec #[-1]
  
  for (i in seq_along(tmp_names)) {
    name <- tmp_names[[i]]
    df <- dplyr::full_join(df, car_list[[name]])
    df[is.na(df)] = as.character(name)
  }
  stats_names <- as.character(unique(df$name))
  store_lst <- make_list(length(stats_names),
                         stats_names)
  
  for (fac in stats_names) {
    store_lst[[fac]] <- df %>%
      dplyr::filter(name == fac) %>%
      subset(select = -c(name))
  }
  return(store_lst)
}
# F: vec_replace() # REPLACE SIG. STRING VECTORS WITH INTERGER VECTORS ####
# Replace significance strings with integers
vec_replace <- function(vec, to_replace, replacement) {
  if (length(to_replace) == length(replacement)) {
    for (i in seq_along(to_replace)) {
      vec <- replace(vec, which(vec == to_replace[[i]]), replacement[[i]])
    }
  } else {
    stop("Please ensure replacement vector is of equal length to vector of values to replace.")
  }
  return(vec)
}
# F: drop_ar_stats() # DROP ALL THE UNWANTED COLUMNS FROM THE AR_STATISTIC TABLES ####
drop_ar_stats <- function(ar_lst, to_keep) {
  # func removes the unwanted cols
  # NOTE: data structure of data.frames in ar_lst must be identical
  keep_vec <- (names(ar_lst[[1]]) %in% to_keep)
  
  for (i in seq_along(ar_lst)) {
    ar_lst[[i]] <- ar_lst[[i]][keep_vec]
  }
  return(ar_lst)
}
# F: ar_lst_signif_repl() # REPLACE SIG. STRING VECTORS WITH INTERGER VACTORS ####
ar_lst_signif_repl <- function(ar_lst, sig_cols, ints=TRUE) {
  for (i in seq_along(ar_lst)) {
    res_df <- ar_lst[[i]]
    for (j in seq_along(sig_cols)) {
      col <- sig_cols[[j]]
      
      if (ints) {
      res_df[[col]] <- vec_replace(res_df[[col]],
                                   c("", "*", "**", "***"),
                                   0:3)
      } else {
        res_df[[col]] <- vec_replace(res_df[[col]],
                                     c("", "*", "**", "***"),
                                     c("NA", "10%", "5%", "1%"))
      }
    }
    ar_lst[[i]] <- res_df
  }
  return(ar_lst)
}
# F: plot_car_stats() # PLOT CAR STATISTICS ####
plot_car_stats <- function(car_lst,
                           grouping,
                           Title = paste0(grouping, ": ", "Event Period ", EVT),
                           name_list = NULL,
                           ar_lst = NULL,
                           Path = NULL,
                           Filename = NULL,
                           rank_sig) {
  # get data
  car_df <- car_lst$car_brown_warner_1985

  # get rank significance
  car_df <- dplyr::full_join(car_df,subset(car_lst$car_rank_test, select=c('significance', 'group')) %>% set_colnames(c('rank.sig', 'group')))
  # Ensure correct dtypes 
  car_df$significance <- factor(car_df$significance,
                                levels = c("NA", "10%", "5%", "1%"),
                                ordered = TRUE)
  car_df$rank.sig <- factor(car_df$rank.sig,
                            levels = c("NA", "10%", "5%", "1%"),
                            ordered = TRUE)

  if (!is.null(ar_lst)) {
    # counts the sample sizes used per group
    sm <- ar_lst %>% sapply(length) %>% as.data.frame() %>% set_colnames('sample.size')
    sm[['group']] <- rownames(sm)
    rownames(sm) <- NULL

    # add sample sizes # dplyr ran into some strange problems
    car_df[['sample.size']] <- NA
    for (i in 1:nrow(sm)) {
      idx <- which(car_df$group == sm[i,]$group)
      car_df[idx,]$sample.size <- sm[i,]$sample.size
      
    }
  } else {}
  # format nicely
  car_df$group <- gsub(pattern = '\\.', replacement = ' ', car_df$group)
  car_df$group <- gsub(".Index", "", car_df$group) %>% as.factor()
  car_df$car_mean <- as.numeric(car_df$car_mean)
  # Reorder
  car_df <- car_df[order(car_df$car_mean),]

  p <-
    ggplot(data = car_df) +
    geom_bar(
      mapping = aes(
        y = group,
        x = sort.int(as.numeric(car_mean) * 100, decreasing = TRUE),
        fill = significance,
        colour = rank.sig
      ),
      size = 1,
      stat = 'identity',
      show.legend = TRUE
    ) +
    theme_bw() +
    labs(
      title = Title,
      subtitle = paste("Cumulative Average Abnormal Returns:",
                       e_meta[[EVT]]$event_window[[1]],
                       " to ",
                       e_meta[[EVT]]$event_window[[length(e_meta[[EVT]]$event_window)]]
                       ),
      caption = "Sample sizes shown as integers.",
      x = "CAAR (%)",
      y = "",
      fill = "Significance"
    ) +
    scale_fill_manual(name = "Parametric \nSignificance",
                      values = RColorBrewer::brewer.pal(4, 'PuBu')) +
    scale_colour_manual(name = "Rank Test \nSignificance",
                        values = RColorBrewer::brewer.pal(4, 'Oranges')) +
    scale_y_discrete(limits = car_df$group,
                     guide = guide_axis(n.dodge = 1)) +
    scale_x_continuous(n.breaks = 20) +
    geom_text(mapping = aes(y = group,
                            x = sort.int(as.numeric(car_mean) * 100, decreasing = TRUE),
                            label = sample.size),
              position = position_dodge2(0))

  
  if (!is.null(Path)) {
    Filename <- paste0(grouping, '_', 'E', EVT, '_car_stats_bar_graph.png')
    ggsave(
      plot = p,
      filename = Filename,
      path = Path,
      dpi = 600,
      width = 12,
      height = 4,
      units = 'in'
    )
  }
  
  return(p)
}

# F: merge_ar_stats() # recombine lists of AR stat data.frames into single df ####
merge_ar_stats <- function(ar_lst){
  # get names
  name_lst <- names(ar_lst)
  # create base df
  df <- ar_lst[[name_lst[[1]]]]
  df[['group']] <- name_lst[[1]]
  
  # remove data already extracted
  rem <- name_lst[-1]
  # loop over names to
  for (name in rem) {
    tmp_df <- ar_lst[[name]]
    tmp_df[['group']] <- name
    df <- rbind.data.frame(df, tmp_df)
  }
  df[['group']] <- as.factor(df[['group']])
  
    for (i in 1:nrow(df)) {
      if ((as.numeric(df[i, 'bh_signif']) >= 2)) {
        df[i, 'text'] <- as.character(df[i, 'group'])
      }
    }
  df$text <- tidyr::replace_na(df$text, "")
  return(df)
}
# F: plot_ar_stats() # Plots bar chart of AR stats #### 
plot_ar_stats <- function(ar_lst,
                           grouping,
                           Title = paste0(grouping, ": ", "Event Period ", EVT),
                           Path = NULL,
                           Filename = paste0(grouping, '_', 'E', EVT, '_ar_stats_point_graph.png'),
                           rank_sig) {
  
  ar_df <- merge_ar_stats(ar_lst = ar_lst)
  # Change labels
  ar_df$bh_signif <- vec_replace(ar_df$bh_signif,
                                 c(0, 1, 2, 3),
                                 c("NA", "10%", "5%", "1%")) %>%
    factor(levels = c("NA", "10%", "5%", "1%"), ordered = TRUE)
  ar_df$gsign_signif <- vec_replace(ar_df$gsign_signif,
                                    c(0, 1, 2, 3),
                                    c("NA", "10%", "5%", "1%")) %>%
    factor(levels = c("NA", "10%", "5%", "1%"), ordered = TRUE)
  ar_df$mrank_signif <- vec_replace(ar_df$mrank_signif,
                                    c(0, 1, 2, 3),
                                    c("NA", "10%", "5%", "1%")) %>%
    factor(levels = c("NA", "10%", "5%", "1%"), ordered = TRUE)
  
  # format nicely
  ar_df$group <- gsub(pattern = '\\.', replacement = ' ', ar_df$group)
  ar_df$group <- gsub(".Index", "", ar_df$group) %>% as.factor()
  ar_df$mean <- as.numeric(round(ar_df$mean*100, 2))
  # Reorder
  # ar_df <- ar_df[order(ar_df$mean),]

  p <- ggplot(data = ar_df) +
    geom_point(
      aes(
        x = date,
        y = group,
        fill = bh_signif,
        color = mrank_signif,
        size = gsign_signif
        # group = mean
      ),
      shape = 21,
      stroke = 2,
      alpha = 0.7,
      show.legend = TRUE
    ) +
    theme_bw() +
    labs(
      title = Title,
      subtitle = paste(
        "Average Abnormal Returns:",
        e_meta[[EVT]]$event_window[[1]],
        " to ",
        e_meta[[EVT]]$event_window[[length(e_meta[[EVT]]$event_window)]]
      ),
      caption = "AARs sizes shown as percentages.",
      x = "Date",
      y = "",
      size = "Significance"
    ) +
    scale_fill_manual(name = "Parametric \nSignificance",
                      values = RColorBrewer::brewer.pal(4, 'Oranges')) +
    scale_colour_manual(name = "Rank Test \nSignificance",
                        values = viridis::plasma(4)) +
    ggrepel::geom_text_repel(
      mapping = aes(
        y = group,
        x = date,
        label = mean
      ),
      box.padding = 0.3,
      nudge_x = 0.2
    )
  # geom_text(mapping = aes(y = group,
  #                         x = date,
  #                         label = round(mean*100,2)),
  #           position = position_dodge2(0),
  #           hjust = -1
  #           )
    
    if (dir.exists(Path)) {
      ggsave(
        plot = p,
        filename = Filename,
        path = Path,
        dpi = 600,
        width = 12,
        height = 8,
        units = 'in'
      )
      
    } else {
      dir.create(path = Path, showWarnings = FALSE)
      ggsave(
        plot = p,
        filename = Filename,
        path = Path,
        dpi = 600,
        width = 12,
        height = 8,
        units = 'in'
      )
    }
  return(p)
}
#############
## META LOOP L1 ####
# Cycles through whole process for "geo" and "ICB"
for (type in 1:2) {
  TYPE <- d_type[[type]]
}

# META LOOP L2 ####
# bin.ar.rtns <- make_list(4, c("event1", "event2", "event3", "event4"))
# bin.car.rtns <- make_list(4, c("event1", "event2", "event3", "event4"))
ar.stats <- make_list(4, c("event1", "event2", "event3", "event4"))
car.stats <- make_list(4, c("event1", "event2", "event3", "event4"))

for(b in seq_along(e_meta)) {
  # bin.ar.rtns[[b]] <- make_list(3,c('geo','indu','supe'))
  # bin.car.rtns[[b]] <- make_list(3,c('geo','indu','supe'))
  
  ar.stats[[b]] <- make_list(3,c('geo','indu','supe'))
  car.stats[[b]] <- make_list(3,c('geo','indu','supe'))
}

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
  ar_geo <- fetch_rtns(name_lst = geo_names,
                       directory = paste0(d_root,
                                          d_geo,
                                          E_DIR,
                                          d_ar))
  # INDUSTRY
  ar_indu <- fetch_rtns(name_lst = indu_names,
                        directory = paste0(d_root,
                                           d_icb,
                                           d_indu,
                                           E_DIR,
                                           d_ar))
  # SUPERSECTOR
  ar_supe <- fetch_rtns(name_lst = supe_names,
                        directory = paste0(d_root,
                                           d_icb,
                                           d_supe,
                                           E_DIR,
                                           d_ar))

  # CUMULATIVE ABNORMAL RETURNS
  # GEOGRAPHIC
  car_geo <- fetch_rtns(name_lst = geo_names,
                        directory = paste0(d_root,
                                           d_geo,
                                           E_DIR,
                                           d_car))
  # INDUSTRY
  car_indu <- fetch_rtns(name_lst = indu_names,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_indu,
                                            E_DIR,
                                            d_car))
  # SUPERSECTOR
  car_supe <- fetch_rtns(name_lst = supe_names,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_supe,
                                            E_DIR,
                                            d_car))

  # AARs: RETRIEVE ALL AAR DATA ####
  # GEOGRAPHIC
  aar_geo <- fetch_rtns(name_lst = geo_names,
                        directory = paste0(d_root,
                                           d_geo,
                                           E_DIR,
                                           d_aar))
  # INDUSTRY
  aar_indu <- fetch_rtns(name_lst = indu_names,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_indu,
                                            E_DIR,
                                            d_aar))
  # SUPERSECTOR
  aar_supe <- fetch_rtns(name_lst = supe_names,
                         directory = paste0(d_root,
                                            d_icb,
                                            d_supe,
                                            E_DIR,
                                            d_aar))
  
  # CAARs: RETRIEVE ALL CAAR DATA ####
  # GEOGRAPHIC
  caar_geo <- fetch_rtns(name_lst = geo_names,
                         directory = paste0(d_root,
                                            d_geo,
                                            E_DIR,
                                            d_caar))
  # INDUSTRY
  caar_indu <- fetch_rtns(name_lst = indu_names,
                          directory = paste0(d_root,
                                             d_icb,
                                             d_indu,
                                             E_DIR,
                                             d_caar))
  # SUPERSECTOR
  caar_supe <- fetch_rtns(name_lst = supe_names,
                          directory = paste0(d_root,
                                             d_icb,
                                             d_supe,
                                             E_DIR,
                                             d_caar))
  
  
  # ARs & CARs: Retrieve STATS data ####
  # DROP UNUSED STATS
  ar_cols_to_keep <- c("date", "weekday", "pct.para", "mean", "bh_stat", "bh_signif", "pct.nonpara", "gsign_stat", "gsign_signif", "mrank_stat", "mrank_signif")
  signif_cols <- c('bh_signif','gsign_signif','mrank_signif')
  # rm_nonpara <- c("rank_test","sign_test", "corrado_sign_test", "wilcoxon_test")
  # rm_para <- c('brown_warner_1980', 'brown_warner_1985', 't_test', 'patell', 'lamb')
  # sar_geo$KOSPI.Index[(names(sar_geo$KOSPI.Index) %in% ar_cols_to_keep)]
  # GEOGRAPHIC
  sar_geo <- fetch_stats(name_lst = geo_names,
                         directory = paste0(d_root,
                                          d_geo,
                                          E_DIR,
                                          d_ar_res)) %>% 
    drop_ar_stats(ar_cols_to_keep)
  sar_geo <- ar_lst_signif_repl(sar_geo, ar_cols_to_keep)
  ar.stats[[E_NAME]]$geo <- sar_geo
  
  # INDUSTRY
  sar_indu <- fetch_stats(name_lst = indu_names,
                          directory = paste0(d_root,
                                           d_icb,
                                           d_indu,
                                           E_DIR,
                                           d_ar_res)) %>% 
    drop_ar_stats(ar_cols_to_keep)
  sar_indu <- ar_lst_signif_repl(sar_indu, ar_cols_to_keep)
  ar.stats[[E_NAME]]$indu <- sar_indu
  
  # SUPERSECTOR
  sar_supe <- fetch_stats(name_lst = supe_names,
                          directory = paste0(d_root,
                                           d_icb,
                                           d_supe,
                                           E_DIR,
                                           d_ar_res)) %>% 
    drop_ar_stats(ar_cols_to_keep)
  sar_supe <- ar_lst_signif_repl(sar_supe, ar_cols_to_keep)
  ar.stats[[E_NAME]]$supe <- ar_supe
  
  sar_cols <- names(sar_geo[[1]])
  
  # CUMULATIVE ABNORMAL RETURNS
  # GEOGRAPHIC
  scar_geo <- fetch_stats(name_lst = geo_names,
                          directory = paste0(d_root,
                                           d_geo,
                                           E_DIR,
                                           d_car_res)) %>% 
    car_stats_tables(geo_names)
  for (i in 1:3) {
    scar_geo[[i]]$significance <- vec_replace(scar_geo[[i]]$significance,
                                              c("","*","**","***"),
                                              c("NA", "10%", "5%", "1%"))
  }
  car.stats[[E_NAME]]$geo <- scar_geo
  
  # INDUSTRY
  scar_indu <- fetch_stats(name_lst = indu_names,
                           directory = paste0(d_root,
                                              d_icb,
                                              d_indu,
                                              E_DIR,
                                              d_car_res)) %>% 
    car_stats_tables(indu_names)
  for (i in 1:3) {
    scar_indu[[i]]$significance <- vec_replace(scar_indu[[i]]$significance,
                                              c("","*","**","***"),
                                              c("NA", "10%", "5%", "1%"))
  }
  car.stats[[E_NAME]]$indu <- scar_indu
  
  # SUPERSECTOR
  scar_supe <- fetch_stats(name_lst = supe_names,
                           directory = paste0(d_root,
                                              d_icb,
                                              d_supe,
                                              E_DIR,
                                              d_car_res)) %>% 
    car_stats_tables(supe_names)
  for (i in 1:3) {
    scar_supe[[i]]$significance <- vec_replace(scar_supe[[i]]$significance,
                                               c("","*","**","***"),
                                               c("NA", "10%", "5%", "1%"))
  }
  car.stats[[E_NAME]]$supe <- scar_supe
  
  scar_cols <- names(scar_geo[[1]])
  
  # Plotting of CAR stats ####
  # Plot CAR results: B&W & RANK
  plot_car_stats(car.stats[[E_NAME]][['geo']],
                 'Geographic',
                 ar_lst = ar_geo,
                 Path = paste0(d_root,
                        d_res_pres,
                        d_plot,
                        E_DIR,
                        d_geo,
                        "aar_caar/"))
  plot_car_stats(car.stats[[E_NAME]][['indu']],
                 'Industry',
                 ar_lst = ar_indu,
                 Path = paste0(d_root,
                               d_res_pres,
                               d_plot,
                               E_DIR,
                               d_icb,
                               d_indu,
                               "aar_caar/"))
  plot_car_stats(car.stats[[E_NAME]][['supe']],
                 'Supersector',
                 ar_lst = ar_supe,
                 Path = paste0(d_root,
                               d_res_pres,
                               d_plot,
                               E_DIR,
                               d_icb,
                               d_supe,
                               "aar_caar/"))
  # plot ar statistics ####
  plot_ar_stats(sar_geo,
                 'Geographic',
                 Path = paste0(d_root,
                               d_res_pres,
                               d_plot,
                               E_DIR,
                               d_geo,
                               "aar_caar/"))
  plot_ar_stats(sar_indu,
                 'Industry',
                 Path = paste0(d_root,
                               d_res_pres,
                               d_plot,
                               E_DIR,
                               d_icb,
                               d_indu,
                               "aar_caar/"))
  plot_ar_stats(sar_supe,
                 'Supersector',
                 Path = paste0(d_root,
                               d_res_pres,
                               d_plot,
                               E_DIR,
                               d_icb,
                               d_supe,
                               "aar_caar/"))
  
  # Merge AAR & CAAR data.frames ####
  # toggle <- function() {

  # GEOGRAPHIC
  ave_lst_geo <- make_list(length(geo_names), geo_names)
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
  ave_lst_indu <- make_list(length(indu_names), indu_names)
  for (i in seq_along(ave_lst_indu)) {
    name <- indu_names[[i]]
    
    temp_df <- merge_caar_aar(aar = aar_indu[[name]],
                              caar = caar_indu[[name]],
                              event_specification = e_meta[[EVT]])
    ave_lst_indu[[name]] <- temp_df
    rm(temp_df)
  }
  # SUPERSECTOR
  ave_lst_supe <- make_list(length(supe_names), supe_names)
  for (i in seq_along(ave_lst_supe)) {
    name <- supe_names[[i]]
    
    temp_df <- merge_caar_aar(aar = aar_supe[[name]],
                              caar = caar_supe[[name]],
                              event_specification = e_meta[[EVT]])
    ave_lst_supe[[name]] <- temp_df
    rm(temp_df)
  }
  
  # PLOT AARs V CAARs ####
  # GEOGRAPHIC
  aar_caar_plot(
    name_lst = geo_names,
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
    name_lst = indu_names,
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
    name_lst = supe_names,
    aar_caar_df_lst = ave_lst_supe,
    PATH = paste0(d_root,
                  d_res_pres,
                  d_plot,
                  E_DIR,
                  d_icb,
                  d_supe,
                  "aar_caar/")
  )
  # }
}

# PLOT ARs ####
for (i in seq_along(geo_names)) {
  indx <- geo_names[[i]]
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
indx <- geo_names[[i]]
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
