#param
library(shiny)
library(shinyWidgets)
library(bslib)
library(httr)
library(rStrava)
library(data.table)
library(lubridate)
library(ggplot2)


#logins and metadata
app_name <- 'AdrienAPI' # chosen by user
app_client_id  <- '32141' # an integer, assigned by Strava
app_secret <- '7fa0351b0424b4f166a109c311b087b3e6f25793' # an alphanumeric secret, assigned by Strava

