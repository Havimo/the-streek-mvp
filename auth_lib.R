get_shiny_url <- function(session){
  # if (interactive()) {
  #   # testing url
  #   APP_URL <- "http://127.0.0.1:6831"
  # } else {
  #   # deployed URL
  #   APP_URL <- "https://havimo.shinyapps.io/the-streek/"
  # }
  if (session$clientData$url_hostname == "127.0.0.1") {
    return(oauth_callback())
  } else {
    app_url <- paste0(session$clientData$url_protocol,
                      "//",
                      session$clientData$url_hostname, 
                      session$clientData$url_pathname,
                      session$clientData$url_port)
    return(app_url)
  }
}

# generate authentication link as set out at https://developers.strava.com/docs/authentication/
# get_authorisation_url <- reactive({
#   paste0('https://www.strava.com/oauth/authorize?client_id={client_id}&response_type=code&redirect_uri={redirect_uri}&approval_prompt=auto&state=')
#   
# })

# parse authentication code from current url if available
get_authorisation_code <- function(session){
  pars <- parseQueryString(session$clientData$url_search)
  
  if (length(pars) > 0) {
    if (!is.null(pars$code)) {
      return(pars$code)
    } else {
      return(NULL)
    }  
  } else {
    return(NULL)
  }
}

# post authorisation code with client id and client secret to get user data
post_authorisation_code <- function(session){
  # authorisation_code <- get_authorisation_code(session)
  # 
  # shiny::validate(
  #   shiny::need(!is.null(authorisation_code),message = '')  
  # )
  # 
  # message(glue('Using authorisation code: {authorisation_code}'))
  # if (!is.null(authorisation_code)) {
    
    response <- POST(url = 'https://www.strava.com/oauth/token',
                     body = list(
                       client_id = APP_CLIENT_ID,
                       client_secret = APP_SECRET,
                       code = authorisation_code
                     )
    )
    return(content(response))  
  # }
  
}

# Get stoken using client id and secret
get_stoken <- function(session){
  token_data <- post_authorisation_code(session)
  accesstoken <- token_data$access_token
  if (!is.null(accesstoken)) {
    stoken <- add_headers(Authorization = paste0("Bearer ",accesstoken))
    
    return(stoken)
  }
}

# output$authUI <- renderUI({
#   if (is.null(get_authorisation_code())) {
#     a('-> To view your activities, click here to authorise access to your Strava data <-',
#       href=paste0('https://www.strava.com/oauth/authorize?client_id=',APP_CLIENT_ID,'&response_type=code&redirect_uri=',APP_URL,'&approval_prompt=auto&state='))
#   } 
# })