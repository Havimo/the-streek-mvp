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
source('auth_lib.R')

thematic::thematic_shiny(font = 'auto')



# Define UI for application that draws a histogram
ui <- fluidPage(
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
    observeEvent(input$connectStrava,{
        # stoken <-  get_stoken(session)
        # stoken <- httr::config(token = strava_oauth(APP_NAME, APP_CLIENT_ID, APP_SECRET, app_scope = "activity:read_all", cache = F))
        cat('generating token with callback uri ', get_shiny_url(session), '\n')
        strava_app <- oauth_app(appname = APP_NAME, key = APP_CLIENT_ID,secret = APP_SECRET, redirect_uri = get_shiny_url(session))
        strava_end <- oauth_endpoint(request = "https://www.strava.com/oauth/authorize?",
                                     authorize = "https://www.strava.com/oauth/authorize",
                                     access = "https://www.strava.com/oauth/token")
        stoken <-  httr::config(token = oauth2.0_token(endpoint = strava_end, app = strava_app, scope = "activity:read_all", cache = F))
        cat('auth success!','\n')
        values <- reactiveValues(myinfo = get_athlete(stoken),
                                 activity_list = GetAndFormatActivityList(stoken))

        output$userImg <- renderUI({
            image <- values$myinfo$profile
            print(image)
            tags$img(src=image, width = 136, height = 136)
        })
        output$currentStreak <- renderText(GetCurrentRunningStreak(values$activity_list, values$myinfo$firstname))

        output$last_16_weeks <- renderPlot(PlotLast16Weeks(values$activity_list,input$plot_type_picker))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
