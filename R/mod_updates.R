# UPDATE NEWSTOPTIMES DATA.FRAME
update_dt <- function(tripid, new_stoptimes, gtfs_data, all_tripids){
  # each trip starts from its own copy of the shape template
  new_stoptimes <- data.table::copy(new_stoptimes)

  # internal test
  # tripid <- "176-1@1#1800" all_tripids[1]
  # add trip_id 
  new_stoptimes[, trip_id := all_tripids[tripid]]
  
  # add cummulative distance
  new_stoptimes[, cumdist := cumsum(dist)]
  
  # subset original stoptimes to get original travel_times btwn stops
  stoptimes_temp <- gtfs_data$stop_times[trip_id == all_tripids[tripid]]
  
  # add departure_time based on stop sequence
  new_stoptimes[stoptimes_temp, on = 'stop_sequence', `:=`(
    'departure_time' = i.departure_time,
    'arrival_time' = i.arrival_time)]
  
  # get a 'stop_sequence' of the stops which have proper info on 'departure_time'
  stop_id_ok <- gtfs_data$stop_times[trip_id == all_tripids[tripid] & 
                                       is.na(departure_time) == FALSE,]$stop_sequence
  
  # ignore trip_id if original departure_time values are missing
  if(is.null(length(stop_id_ok)) == TRUE | length(stop_id_ok) == 1 | length(stop_id_ok) == 0){ 
    message(paste0("Trip '", all_tripids[tripid], "' has less than two stop_ids. Ignoring it.")) # nocov
    return(NULL) # nocov
  }
  
  new_stoptimes[, speed := numeric()]
  
  # lim0: 'id' in which stop_times intervals STARTS
  lim0 <- new_stoptimes[ !is.na(departure_time) & !is.na(stop_id), id]
  
  new_points <- data.table::copy(new_stoptimes[lim0, ])
  new_points[, departure_time := arrival_time]
  new_points[, id := id - 0.1]
  
  new_stoptimes[lim0, dist := 0]
  
  new_stoptimes <- rbind(new_stoptimes, new_points)
  data.table::setorder(new_stoptimes, "id")
  new_stoptimes$id <- 1:dim(new_stoptimes)[1]
  
  new_stoptimes[, timestamp := data.table::as.ITime(departure_time)]
  
  new_stoptimes[1, speed := 1e-12]
  new_stoptimes[lim0 + 1, speed := 1e-12]
  new_stoptimes[, cumtime := 0]
  new_stoptimes[, time := 0]
  
  last_point_was_stop <- FALSE
  
  lim0 <- new_stoptimes[ !is.na(timestamp) & !is.na(stop_id), id]
  #  function for speed estimation
  update_speeds <- function(i){
    a <- lim0[i]
    b <- lim0[i + 1]
    
    diff_timestamp <- new_stoptimes$timestamp[b] - new_stoptimes$timestamp[a]
    if(diff_timestamp < 0) diff_timestamp <- diff_timestamp + 86400 # one day in seconds
    
    if(a + 1 == b && !last_point_was_stop) {
      last_point_was_stop <<- TRUE
      value <- new_stoptimes[a, cumtime] + diff_timestamp
      new_stoptimes[b, cumtime := value]
      new_stoptimes[b, speed := 1e-12]
      return() # two consecutive points with arrival_time don't need to be interpolated
    }
    
    new_speed <- (new_stoptimes$cumdist[b] - new_stoptimes$cumdist[a]) / as.numeric(diff_timestamp) # m/s
    
    new_stoptimes[a:b, speed := 3.6 * new_speed] # km/h
    
    time_a <- new_stoptimes[a, cumtime]
    
    new_stoptimes[a:b, time := (cumdist - data.table::shift(cumdist, 1)) / new_speed]
    
    new_stoptimes[a, time := time_a] # necessary because the shift above will produce NA
    
    new_stoptimes[a:b, cumtime := cumsum(time)]
    
    new_stoptimes[a, speed := 1e-12]
    
    new_stoptimes[a:(b-1), timestamp := data.table::first(timestamp) + round(cumtime - data.table::first(cumtime))]
    last_point_was_stop <<- FALSE
  }
  
  lapply(1:(length(lim0) - 1), FUN = update_speeds)
  
  new_stoptimes[is.na(speed), cumtime := NA]
  new_stoptimes[, time := NULL]
  
  # Get lag
  #new_stoptimes[!is.na(departure_time) & !is.na(stop_id)
  #              ,lag := departure_time - arrival_time]
  #new_stoptimes[is.na(lag), lag := 0]
  # Speed info that was missing (either before or after 1st/last stops)
  # Get trip duration in seconds
  #  new_stoptimes[, cumtime := cumsum(3.6 * dist / speed)]
  
  new_stoptimes[, trip_number := tripid]

  # reorder columns
  data.table::setcolorder(new_stoptimes, c("trip_id", "route_type", "id", 
                                           "shape_pt_lon", "shape_pt_lat", 
                                           "departure_time", "stop_id", 
                                           "stop_sequence", "dist", "cumdist",
                                           "speed", "cumtime"))
  
  # distance from trip start to 1st stop
  #  dist_1st <- new_stoptimes[id == lim0[1]]$cumdist # in m
  
  # get the depart/arrival time from 1st stop
  #departtime_1st <- as.numeric(new_stoptimes[id == lim0[1]]$departure_time)
  #departtime_1st <- departtime_1st - (3.6 * dist_1st / new_stoptimes$speed[1]) # time in seconds
  #  arrival_1st <- as.numeric(new_stoptimes[id == lim0[1]]$arrival_time)
  #  arrival_1st <- arrival_1st - (3.6 * dist_1st / new_stoptimes$speed[1]) # time in seconds
  
  
  # Determine the start time of the trip (time stamp the 1st GPS point of the trip)
  #suppressWarnings(new_stoptimes[id == 1, departure_time := round(departtime_1st)])
  #  suppressWarnings(new_stoptimes[id == lim0[1], arrival_time := round(arrival_1st)]) 
  
  # recalculate time stamps, except the given 'departure_time's from stop sequences
  #stop_id_nok <- which(is.na(new_stoptimes$departure_time))
  # update indexes in 'newstoptimes'
  # new_stoptimes[, departure_time := departure_time[lim0[1]] +  cumtime + cumsum(lag)]
  #  new_stoptimes[, arrival_time := departure_time - lag]
  
  # round
  #  new_stoptimes[, timestamp := round(timestamp)]
  #  new_stoptimes[, arrival_time := round(arrival_time)]
  
  if(is.null(new_stoptimes)){
    message(paste0("Could not create stop times for trip '", 
                   all_tripids[tripid], "'. Ignoring it.")) # nocov
  }
  else if(dim(new_stoptimes)[1] == 0)
    message(paste0("Trip '", all_tripids[tripid], 
                   "' has zero GPS points. Ignoring it.")) # nocov
  
  return(new_stoptimes)
}