library(ggplot2)
library(magrittr)
library(Polychrome)
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/event_spec.R")
source("C:/Users/Keegan/Desktop/Repository/@ Development/Estudy_R/development_aid_functions.R")

# CONSTANTS ####
E <- 4
NUM_IDX <- 22
SCALE <- 100

# META VARIABLES ####
j.cal.uni <-
  rjson::fromJSON(file = "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/calendars/weekdays.json")
create.calendar(
  j.cal.uni$name,
  holidays = j.cal.uni$holidays,
  weekdays = j.cal.uni$weekdays,
  start.date = as.Date("2018-01-01"),
  end.date = as.Date("2020-12-31"),
  adjust.from = j.cal.uni$adjust.from,
  adjust.to = j.cal.uni$adjust.to,
  financial = j.cal.uni$financial
)

e_meta <- make_list(4, c(c("event1", "event2", "event3", "event4")))
e_meta[[1]] <- event_spec(
  e_name = "event1",
  grouping = "meta",
  edate = as.Date("2020-01-13"),
  bounds = c(-5, 5),
  est_len = 250,
  calendar = "weekdays"
)
e_meta[[2]] <- event_spec(
  e_name = "event2",
  grouping = "meta",
  edate = as.Date("2020-01-23"),
  bounds = c(-2, 8),
  est_len = 250,
  calendar = "weekdays"
)
e_meta[[3]] <- event_spec(
  e_name = "event3",
  grouping = "meta",
  edate = as.Date("2020-02-24"),
  bounds = c(-1, 9),
  est_len = 250,
  calendar = "weekdays"
)
e_meta[[4]] <- event_spec(
  e_name = "event4",
  grouping = "meta",
  edate = as.Date("2020-03-09"),
  bounds = c(-1, 9),
  est_len = 250,
  calendar = "weekdays"
)

# DIRECTORIES ####
d_main <- "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/"
d_group <- "geographic_region/"
d_event <- "event"
d_aar <- "aar/"

directories <- vector(E, mode = "list")
for (i in 1:length(directories)) {
  d <- paste0(d_main, d_group, d_event, i, "/", d_aar)
  directories[[i]] <- d %>%
    list.files %>%
    sapply(function(x) {
      paste0(d, x)
    })
}
filenames <- vector(NUM_IDX, mode = "list")
filenames <-
  list.files(paste0(d_main, d_group, d_event, 1, "/", d_aar)) %>%
  stringr::str_remove("\\.csv")

# GET DATA ####
# create storage obj
data <- vector(mode = "list", length = E) %>%
  set_names(c("event1", "event2", "event3", "event4"))
for (i in 1:length(data)) {
  data[[i]] <- vector(mode = "list", length = NUM_IDX)
}
# read data
for (i in 1:length(directories)) {
  for (j in 1:length(directories[[i]])) {
    if (j == 1) {
      df <- read.csv(directories[[i]][[j]]) %>% as.data.frame()
      df[["Group"]] <- colnames(df)[[2]]
      colnames(df) <- c("Date", "AAR", "Group")
    } else {
      file <- read.csv(directories[[i]][[j]]) %>% as.data.frame()
      file[["Group"]] <- colnames(file)[[2]]
      colnames(file) <- c("Date", "AAR", "Group")
      df <- rbind.data.frame(df, file)
    }
  }
  data[[i]] <- df
  rm(df)
}

# PLOT DATA ####
for (EVT in 1:length(e_meta)) {
  grouping <- "Geographic"
  event_data <- data[[EVT]]
  RANGE <-
    (abs(floor(min(
      event_data$AAR * SCALE
    ))) + abs(ceiling(max(
      event_data$AAR * SCALE
    ))))
  colours <-
    Polychrome::colorNames(Polychrome::alphabet.colors(length(unique(event_data$Group))))
  p <-
    ggplot(data = event_data) +
    geom_line(
      mapping = aes(
        x = as.Date(Date),
        y = AAR * SCALE,
        colour = Group
      ),
      # label = 'Average Abnormal Return',
      show.legend = TRUE
    ) +
    theme_bw() +
    theme(plot.margin = unit(c(1, 1, 2.25, 1), "lines")) +
    scale_colour_manual(values = colours) +
    geom_hline(yintercept = 0,
               size = 0.3) +
    labs(
      title = paste0("Event ", EVT, ": ", grouping),
      subtitle = paste(
        "Average Abnormal Returns:",
        e_meta[[EVT]]$event_window[[1]],
        " to ",
        e_meta[[EVT]]$event_window[[length(e_meta[[EVT]]$event_window)]]
      ),
      x = "Date",
      y = "Average Abnormal Return (%)",
    ) +
    scale_x_date(
      breaks = "day",
      guide = guide_axis(n.dodge = 2)
      # labels = as.Date("%Y-%m-%d")
      ) +
      scale_y_continuous(breaks = seq.int(
        from = floor(min(event_data$AAR * SCALE)),
        to = ceiling(max(event_data$AAR *
                           SCALE)),
        by = 1 / RANGE
      ))
  # Store plot
  PATH <-
    "C:/Users/Keegan/OneDrive/1 Studies/2021 - 2022/5003W/3 - Dissertation/5-Data/results/estudy/geographic_region/aar_plots/"
  ggsave(
    p,
    filename = paste0("E", EVT, "_AAR_plot__geographic.png"),
    path = PATH,
    dpi = 600,
    width = 15,
    height = 7.5,
    units = 'in'
  )
}
