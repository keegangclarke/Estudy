merge_caar_aar <-
  function(storage_lst,
           name_lst,
           aar_lst,
           caar_lst,
           event_specification,
           evt_day = NULL) {
    count <- 0
    # "storage_lst" == empty list of correct length to store merged data.frames
    # "name_lst" == list of names of groupings
    # "aar_lst" & "caar_lst" == lists of AARs and CAARs
    # "evt_day" == event_spec day by which 'Event.Time' will be calculated
    
    evt_day <- event_specification$event_date
    e_time_df <- setNames(as.data.frame(event_specification$event_window),"Date")
    # calculates sequence of ints that represent the event-time
    e_time_df$Event.Time <- -sum(as.integer(e_time_df[["Date"]] < evt_day)):sum(as.integer(e_time_df[["Date"]] > evt_day))
    
    if (is.null(evt_day)) {
      message("No event day specified. Please specify an event day in standard format.")
      
    } else {
      for (i in seq_along(name_lst)) {
        name <- name_lst[[i]]
        print(name)
        print(i)
        
        tempDF <- merge.data.frame(aar_lst[[name]],
                                   caar_lst[[name]],
                                   by = "Date") %>%
          set_names(c('Date', "AAR", "CAAR"))
        tempDF <- merge.data.frame(tempDF,e_time_df,by = "Date")
        # adds event time column but calculates on logical basis an int vector which sets 0 to 'E_DAY'
        # negative ints for days before 'E_DAY'
        # postive ints for days after 'E_DAY'
        
        
        e_time <-
          as.integer(-nrow(aar_lst[[name]][(aar_lst[[name]][["Date"]] < evt_day),]):nrow(aar_lst[[name]][(aar_lst[[name]][["Date"]] > evt_day),]))
        
        # Specifies expected event time, for positive and negative time
        # NOTE: ignore 0 as unnecessary for testing. 0 == event day and is included later
        neg <-
          -nrow(aar_lst[[name]][(aar_lst[[name]][["Date"]] < evt_day), ]):0
        pos <-
          1:nrow(aar_lst[[name]][(aar_lst[[name]][["Date"]] > evt_day), ])
        # Calcs actual length of obs that are present in df
        neg_row <- sum(as.integer(tempDF$Date < evt_day))
        pos_row <- sum(as.integer(tempDF$Date > evt_day))
        # Tests if lengths are identical or not
        if (any((length(neg) == 1) & (neg == 0))) {
          t_neg <- identical(neg, neg_row)
        } else {
          t_neg <- identical(length(head(neg,-1)), neg_row)
        }
        
        t_pos <- identical(length(pos), pos_row)
        
        # logic that then adjusts 'neg' & 'pos' prior to creating 'e_time'
        if (t_neg & t_pos) {
          if ((evt_day %in% tempDF$Date) == FALSE) {
            if (any((length(neg) == 1) & (neg == 0))) {
              neg <- NULL
              e_time <- union(neg, pos)
            } else {
              e_time <- union(neg, pos)
            }
          } else {
            e_time <- union(union(neg, 0), pos)
          }
          
          # adds event time to df
          tempDF$Event.Time <- e_time
          storage_lst[[name]] <- tempDF
          
        } else if ((t_neg == FALSE) & (t_pos == TRUE)) {
          dif <- neg_row - length(neg)
          neg <- tail(neg, diff)
          
          if ((evt_day %in% tempDF$Date) == FALSE) {
            e_time <- union(neg, pos)
          } else{
            e_time <- union(union(neg, 0), pos)
          }
          
          # adds event time to df
          tempDF$Event.Time <- e_time
          storage_lst[[name]] <- tempDF
          
        } else if ((t_neg == TRUE) & (t_pos == FALSE)) {
          if (any(neg == 0)) {
            neg <- NULL
            dif <- pos_row - length(pos)
            pos <- head(pos, diff)
            
            e_time <- union(neg, pos)
          } else {
            dif <- pos_row - length(pos)
            pos <- head(pos, diff)
          }
          
          if ((evt_day %in% tempDF$Date) == FALSE) {
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