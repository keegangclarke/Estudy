# Load packages & functions
library(magrittr)
library(ggplot2)
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

# 1. Parameters ####
# list to store all event params
all_events <- make_list(4, 
                        c("event1", "event2", "event3", "event4"))

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
  "event1",
  edate = as.Date("2020-01-13"),
  bounds = c(-5, 5),
  est_len = 250
)
all_events[[2]] <- event_spec(
  "event2",
  edate = as.Date("2020-01-24"),
  bounds = c(-2, 8),
  est_len = 250
)
all_events[[3]] <- event_spec(
  "event3",
  edate = as.Date("2020-02-24"),
  bounds = c(-1, 9),
  est_len = 250
)
all_events[[4]] <- event_spec(
  "event4",
  edate = as.Date("2020-03-09"),
  bounds = c(-1, 9),
  est_len = 250
)

# Directory components ####
d_root <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"

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
d_car <- "car/"
d_caar <- "caar/"

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

# FUNCTION TO MERGE CAAR & AAR DATA LISTS TOGETHER FOR PLOTS ####
merge_caar_aar <-
  function(storage_lst,
           name_lst,
           aar_lst,
           caar_lst,
           evt_day = NULL) {
    
    count <- 0
    # "storage_lst" == empty list of correct length to store merged data.frames
    # "name_lst" == list of names of groupings
    # "aar_lst" & "caar_lst" == lists of AARs and CAARs
    # "evt_day" == event day by which 'Event.Time' will be calculated
    if (is.null(evt_day)) {
      message("No event day specified. Please specify an event day in standard format.")
      
    } else {
      for (i in seq_along(name_lst)) {
        name <- name_lst[[i]]
        
        tempDF <- merge.data.frame(aar_lst[[name]],
                                   caar_lst[[name]],
                                   by = "Date") %>%
          set_names(c('Date', "AAR", "CAAR"))
        # adds event time column but calculates on logical basis an int vector which sets 0 to 'E_DAY'
        # negative ints for days before 'E_DAY'
        # postive ints for days after 'E_DAY'
        e_time<- as.integer(
          -nrow(aar_lst[[name]][(aar_lst[[name]][["Date"]] < evt_day), ]):nrow(aar_lst[[name]][(aar_lst[[name]][["Date"]] > evt_day), ])
        )
        
        # Specifies expected event time, for positive and negative time
        # NOTE: ignore 0 as unnecessary for testing. 0 == event day and is included later
        neg <- -nrow(aar_lst[[name]][(aar_lst[[name]][["Date"]] < evt_day),]):-1
        pos <-  1:nrow(aar_lst[[name]][(aar_lst[[name]][["Date"]] > evt_day),])
        # Calcs actual length of obs that are present in df
        neg_row <- sum(as.integer(tempDF$Date < evt_day))
        pos_row <- sum(as.integer(tempDF$Date > evt_day))
        # Tests if lengths are identical or not
        t_neg <- identical(length(neg), neg_row)
        t_pos <- identical(length(pos), pos_row)
        
        # logic that then adjusts 'neg' & 'pos' prior to creating 'e_time'
        if (t_neg & t_pos) {
          if ((evt_day %in% tempDF$Date)==FALSE) {
            e_time <- union(neg, pos)
          } else{
            e_time <- union(union(neg, 0), pos)
          }
          
          # adds event time to df
          tempDF$Event.Time <- e_time
          storage_lst[[name]] <- tempDF
          
        } else if ((t_neg == FALSE) & (t_pos == TRUE)) {
          dif <- neg_row - length(neg)
          neg <- tail(neg, diff)
          
          if ((evt_day %in% tempDF$Date)==FALSE) {
            e_time <- union(neg, pos)
          } else{
            e_time <- union(union(neg, 0), pos)
          }
          
          # adds event time to df
          tempDF$Event.Time <- e_time
          storage_lst[[name]] <- tempDF
          
        } else if ((t_neg == TRUE) & (t_pos == FALSE)) {
          dif <- pos_row - length(pos)
          pos <- head(pos, diff)
          
          if ((evt_day %in% tempDF$Date)==FALSE) {
            e_time <- union(neg, pos)
          } else{
            e_time <- union(union(neg, 0), pos)
          }
          
          # adds event time to df
          tempDF$Event.Time <- e_time
          storage_lst[[name]] <- tempDF
          
        } else {
          message("Could not achieve correct event-time for item")
          print(i)
          print(name)
          message("Please debug as event-time was subsequently not specified.")
        }
        count <- count + 1
      }
    }
    
    if (count == length(storage_lst)) {
      print("Merger complete.")
    } else {
      message("Something went wrong. Insufficient elements modified.")
      print(cat("Total modified: ", i))
    }
    
    return(storage_lst)
  }


## META LOOP L1 ####
# Cycles through whole process for "geo" and "ICB"
for (type in 1:2) {
  TYPE <- d_type[[type]]
}

# META LOOP L2 ####
for (EVT in seq_along(all_events)) {
  # SPECIFY PARAMS
  E_NAME <- all_events[[EVT]]$event_name
  E_DAY <- all_events[[EVT]]$event_date
  E_WIND <- all_events[[EVT]]$event_window
  EST_WIND <- all_events[[EVT]]$estimation_window
  
  E_DIR <- paste0(all_events[[EVT]]$event_name,"/")

}

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


# Merge AAR & CAAR data.frames ####
# GEOGRAPHIC
ave_lst_geo <- make_list(length(geo_fac), geo_fac)
ave_lst_geo <- merge_caar_aar(
  storage_lst = ave_lst_geo,
  name_lst = geo_fac,
  aar_lst = aar_geo,
  caar_lst = caar_geo,
  evt_day = E_DAY
)
# INDUSTRY
ave_lst_indu <- make_list(length(indu_fac), indu_fac)
ave_lst_indu <- merge_caar_aar(
  storage_lst = ave_lst_indu,
  name_lst = indu_fac,
  aar_lst = aar_indu,
  caar_lst = caar_indu,
  evt_day = E_DAY
)
# SUPERSECTOR
ave_lst_supe <- make_list(length(supe_fac), supe_fac)
ave_lst_supe <- merge_caar_aar(
  storage_lst = ave_lst_supe,
  name_lst = supe_fac,
  aar_lst = aar_supe,
  caar_lst = caar_supe,
  evt_day = E_DAY
)

# PLOT AARs V CAARs
aar_caar_plot <- function(name_lst, aar_caar_df_lst, PATH = NULL) {
  for (i in seq_along(name_lst)) {
    nam <- name_lst[[i]]
    # AAR vs CAAR PLOT ####
    p_ave <-
      ggplot(data = aar_caar_df_lst[[nam]]) +
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
      scale_x_continuous(breaks = round(seq(min(aar_caar_df_lst[[nam]]$Event.Time),
                                            max(aar_caar_df_lst[[nam]]$Event.Time),
                                            by = 10),
                                        1)) +
      scale_y_continuous(breaks = round(seq(min(aar_caar_df_lst[[nam]]$Event.Time),
                                            max(aar_caar_df_lst[[nam]]$Event.Time),
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
                         nam),)
    
    # Store plot
    ggsave(
      p_ave,
      filename = paste0(nam, "_aar_caar.png"),
      path = PATH,
      dpi = 320,
      width = 8,
      height = 4,
      units = 'in'
    )
  }
}

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
                d_icb,
                d_supe,
                "aar_caar/")
)


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
