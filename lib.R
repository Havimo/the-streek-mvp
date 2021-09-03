GetAndFormatActivityList <- function(stoken){
  cat('Downloading activity list....')
  my_acts <- get_activity_list(stoken)
  
  
  final_dt <- rbindlist(lapply(my_acts,function(x) as.data.table(t(unlist(x)))), use.names = TRUE, fill = TRUE)
  cols_to_keep <- c("athlete.id", "id", "name", "distance", "moving_time", "elapsed_time", "total_elevation_gain", "type", "workout_type", "start_date",             
                     "start_latlng1", "start_latlng2", "end_latlng1","end_latlng2","location_country",                        
                    "achievement_count","kudos_count","map.id","map.summary_polyline", "gear_id","average_cadence",
                    "average_heartrate","max_heartrate" ,"elev_high","elev_low","suffer_score")
  final_dt <- final_dt[, ..cols_to_keep]
  setnames(final_dt,c("athlete.id","id","type","start_latlng1", "start_latlng2", "end_latlng1","end_latlng2","map.id","map.summary_polyline",'start_date'),
                    c("athlete_id","activity_id","activity_type","start_lat","start_long","end_lat","end_long","map_id","map_polyline",'start_date'))
  final_dt[,start_time := substr(start_date,12,19)]
  final_dt[,start_date := ymd(substr(start_date,1,10))]
  
  num_cols <- c("athlete_id", "activity_id", "distance", "moving_time", "elapsed_time", "total_elevation_gain",             
                     "start_lat","start_long","end_lat","end_long",                     
                    "achievement_count","kudos_count","average_cadence",
                    "average_heartrate","max_heartrate" ,"elev_high","elev_low","suffer_score")
  final_dt[,(num_cols) := lapply(.SD, as.numeric), .SDcols = num_cols]
  
  return(final_dt)
}

GetCurrentRunningStreak <-  function(activity_list, name){
  streak_dt <- activity_list[activity_type == 'Run',.N,.(start_date)]
  
  if(nrow(streak_dt[start_date == today()]) == 0) {
    ran_today <- FALSE
  } else {
    ran_today <- TRUE
  }
  
  if(nrow(streak_dt[start_date == today() - 1]) == 0) {
    ran_yesterday <- FALSE
  } else {
    ran_yesterday <- TRUE
  }
  
  setorder(streak_dt, start_date)
  streak_dt[, streaking := ifelse(as.numeric(start_date - shift(start_date,1)) == 1, 1, 0)]
  streak_dt[, streak_length := rowid(rleid(streaking)) + 1]
  streak_dt[is.na(streaking) | streaking != 1, streak_length := 0]
  
  max_streak <-  max(streak_dt$streak_length)
  
  if(ran_yesterday){

    current_streak <-  streak_dt[start_date == today() - 1]$streak_length + (ran_today * 1)
    string <-  paste("Your current streak is", current_streak, 'days!')
    if(ran_today) string <- paste(string, "You're all set for today, get some rest",name,".")
    if(!ran_today) string <- paste(string, "You still need to run today, get out there",name,"!")
  }
  
  if(!ran_yesterday & !ran_today){
    string <- "You're not on a streak currently."
  }
  
  string <- paste(string, "\n Your longest streak is", max_streak, "days.")
  
  return(string)
}

PlotLast16Weeks <-  function(final_dt, type = 'distance'){
  dataplot <- final_dt[,.(distance = sum(distance), moving_time = sum(moving_time), suffer_score = sum(suffer_score)),.(Week = lubridate::ceiling_date(start_date), activity_type)]
  plot <- ggplot(dataplot[Week >= today() - 16*7]) + 
    geom_col(aes_string(x = 'Week', y = type, fill = 'activity_type')) + 
    labs(title = paste('Last 16 weeks', type), y = type, x = '')
  
  return(plot)
}
