library(rStrava)
library(ggmap)
library(data.table)

#logins and metadata
app_name <- 'AdrienAPI' # chosen by user
app_client_id  <- '32141' # an integer, assigned by Strava
app_secret <- '7fa0351b0424b4f166a109c311b087b3e6f25793' # an alphanumeric secret, assigned by Strava
my_athlete_id <- 16473726

# create the authentication token
stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all", cache = T))

myinfo <- get_athlete(stoken)

GetAndFormatActivityList <- function(stoken){
  
  my_acts <- get_activity_list(stoken)
  
  
  final_dt <- rbindlist(lapply(my_acts,function(x) as.data.table(t(unlist(x)))), use.names = TRUE, fill = TRUE)
  cols_to_keep <- c("athlete.id", "id", "name", "distance", "moving_time", "elapsed_time", "total_elevation_gain", "type", "workout_type", "start_date",             
                     "start_latlng1", "start_latlng2", "end_latlng1","end_latlng2","location_country",                        
                    "achievement_count","kudos_count","map.id","map.summary_polyline", "gear_id","average_cadence",
                    "average_heartrate","max_heartrate" ,"elev_high","elev_low","suffer_score")
  final_dt <- final_dt[, ..cols_to_keep]
  setnames(final_dt,c("athlete.id","id","type","start_latlng1", "start_latlng2", "end_latlng1","end_latlng2","map.id","map.summary_polyline"),
                    c("athelete_id","activity_id","activity_type","start_lat","start_long","end_lat","end_long","map_id","map_polyline"))
  return(final_dt)
}

# final_dt <- final_dt[,c("gear_name",'brand_name','gear_distance','gear_retired')
#                           := get_gear(gear_id,stoken)[c("model_name","brand_name","distance","retired")], .(athelete_id, activity_id)]


