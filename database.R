source('param.R')
source('lib.R')
source('R/utils.R')

stoken <- httr::config(token = strava_oauth(APP_NAME, APP_CLIENT_ID, APP_SECRET, app_scope="activity:read_all", cache = T))

myinfo <- get_athlete(stoken)

activity_list  <- GetAndFormatActivityList(stoken, F)


drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv,
                 dbname="streek_prod",
                 host='34.65.115.156',
                 port=5432,
                 user='streek_etl',
                 password='')

dbExecute(con, 'DROP TABLE dim_user CASCADE')
dbExecute(con, 'DROP TABLE fact_streek_summary')
dbExecute(con, read('SQL/dim_user.sql'))
dbExecute(con, read('SQL/streek_summary.sql'))

#insert a user if it doesn't exist
dbExecute(con, paste0("INSERT INTO dim_user VALUES (
                       ", myinfo$id, ",
                       '", myinfo$firstname, "',
                       '", myinfo$lastname, "',
                       '", myinfo$city, "',
                       '", myinfo$country, "',
                       '", myinfo$profile_medium, "');"))

data <- GetRunningStreakData(activity_list,'Run')[[1]]
data[,user_id := myinfo$id]
setcolorder(data,c('user_id','streek_type','current_streek_value','max_streek_value'))

append_cmd  <-  sqlAppendTable( con = con , table = 'fact_streek_summary' , values = data , row.names = FALSE )
dbExecute( conn = con , statement = append_cmd )
dbGetQuery(con,"SELECT * FROM fact_streek_summary")
