#param
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(bslib)
library(httr)
library(rStrava)
library(data.table)
library(lubridate)
library(ggplot2)


#global variables
APP_NAME <- 'AdrienAPI' # chosen by user
APP_CLIENT_ID  <- '32141' # an integer, assigned by Strava
APP_SECRET <- '7fa0351b0424b4f166a109c311b087b3e6f25793' # an alphanumeric secret, assigned by Strava
APP_URL <- "https://havimo.shinyapps.io/the-streek"
# APP_URL <- 'https://havimo.shinyapps.io/the-streek/'
# APP_URL <- 'http://127.0.0.1:6831'

