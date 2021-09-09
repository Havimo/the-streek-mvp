# Libraries
library(rStrava)
library(jsonlite)
library(httr)

# Strava API info
app_name <- 'AdrienAPI' # chosen by user
app_client_id  <- '32141' # an integer, assigned by Strava
app_secret <- '7fa0351b0424b4f166a109c311b087b3e6f25793' # an alphanumeric secret, assigned by Strava
app_url <- "https://havimo.shinyapps.io/the-streek"


# Define the UI
ui <- fluidPage(
  uiOutput('authUI'),
  br(),
  textOutput('auth_success'),
  downloadLink('downloadData', 'Download all activities')
)


# Define the server code
server <- function(input, output,session) {
  
  appURL <- reactive({
    
    if(!is.null(session)) {
      app_url <- paste0(session$clientData$url_protocol,
                        "//",
                        session$clientData$url_hostname, 
                        ifelse(
                          session$clientData$url_hostname == "127.0.0.1", 
                          ":",
                          session$clientData$url_pathname
                        ),
                        session$clientData$url_port
      )
      return(app_url)
    }
  })
  
  # Generate authentication link
  authURL <- reactive({
    paste('https://www.strava.com/oauth/authorize?','client_id=',app_client_id,'&response_type=code&redirect_uri=',appURL(),'&approval_prompt=force&scope=activity:read_all&state=',sep='',collapse='')
  })

  # Parse out the authentication code from the url
  authCode <- reactive({
    pars <- parseQueryString(session$clientData$url_search) 
    cat('code  :',pars$code)
    if(length(pars$code) > 0){
      return(pars$code)
    } else {
      return(NULL)
    } 
  })
  
  # Get stoken
  get_stoken <- reactive({
    code <- authCode()
    cat('posting response')
    response <- POST(url = 'https://www.strava.com/oauth/token',
                    body = list(client_id = app_client_id,
                                client_secret = app_secret,
                                code = code))
    token_data <- content(response)
    accesstoken <- token_data$access_token
    stoken <- add_headers(Authorization = paste0("Bearer ",accesstoken))
    return(stoken)
  })
  
  output$authUI <- renderUI({
    if (is.null(authCode())) {
      cat(authURL())
      a('Click to authorise Strava Access',href=authURL())
    }
  })
  
  # Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('StravaActivities-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      # get and save all activities
      my_acts <- get_activity_list(get_stoken())
      
      # Convert activities list to data frame
      my_acts.df <- compile_activities(my_acts, acts = NULL, units = "metric")
      
      write.csv(my_acts.df, con, row.names = FALSE)
    }
  )
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)