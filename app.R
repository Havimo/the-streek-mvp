#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source('param.R')
source('lib.R')
# source('auth_lib.R')

thematic::thematic_shiny(font = 'auto')

js_code <- "
    shinyjs.jsBrowseURL = function(url) {
      window.open(url,'_blank');
    }
"


# Define UI for application that draws a histogram
ui <- fluidPage(
    # set up shiny js to be able to call our browseURL function
    useShinyjs(),
    extendShinyjs(text = js_code, functions = 'jsBrowseURL'),
    
    theme = bs_theme(version = 4, bootswatch = "minty",
                   base_font = font_google("DM Sans"),
                  code_font = font_google("Space Mono")),

    # Application title
    titlePanel("The Streek"),
    shinybusy::add_busy_spinner(spin = "cube-grid"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # uiOutput("authUI")
          actionButton("connectStrava","Connect to Strava")
        ),
        # Show a plot of the generated distribution
        mainPanel(
            uiOutput("userImg", click = "MyImage"),
            textOutput("currentStreak"),
            pickerInput(inputId = "plot_type_picker",label = "Select metric to plot", choices = c('distance','moving_time','suffer_score'),
                       multiple = FALSE, selected = "distance"),
            plotOutput("last_16_weeks")
        )
    )
)
server <- function(input, output, session) {
    # output$authUI <- renderUI({
    #   if (is.null(get_authorisation_code(session))) {
    #     a('-> To view your activities, click here to authorise access to your Strava data <-',
    #       href=paste0('https://www.strava.com/oauth/authorize?client_id=',APP_CLIENT_ID,'&response_type=code&redirect_uri=',APP_URL,'&approval_prompt=auto&state='))
    #   }
    # })
    logins <-  reactiveValues(stoken = NULL, code = NULL)
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
        paste('https://www.strava.com/oauth/authorize?','client_id=',APP_CLIENT_ID,'&response_type=code&redirect_uri=',appURL(),'&approval_prompt=force&scope=activity:read_all&state=',sep='',collapse='')
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
                         body = list(client_id = APP_CLIENT_ID,
                                     client_secret = APP_SECRET,
                                     code = code))
        token_data <- content(response)
        accesstoken <- token_data$access_token
        stoken <- add_headers(Authorization = paste0("Bearer ",accesstoken))
        return(stoken)
    })

    observeEvent(input$connectStrava,{
      # stoken <-  get_stoken(session)
      # stoken <- httr::config(token = strava_oauth(APP_NAME, APP_CLIENT_ID, APP_SECRET, app_scope = "activity:read_all", cache = F))
      js$jsBrowseURL(authURL())
    })
    
    observe({
      req(authCode())
      if(!is.null(authCode())){
        stoken <- get_stoken()
        cat('auth success!','\n')
        
        values <- reactiveValues(myinfo = get_athlete(stoken),
                                 activity_list = GetAndFormatActivityList(stoken, F))
        
        output$userImg <- renderUI({
          image <- values$myinfo$profile
          print(image)
          tags$img(src=image, width = 136, height = 136)
        })
        output$currentStreak <- renderText(GetCurrentRunningStreak(values$activity_list, values$myinfo$firstname))
        
        output$last_16_weeks <- renderPlot(PlotLast16Weeks(values$activity_list,input$plot_type_picker))
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
