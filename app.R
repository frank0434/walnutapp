#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(magrittr)
library(ggplot2)
library(clifro)

# Define UI for application that draws a histogram ----------
ui <- fluidPage(

    # Application title
    titlePanel("Walnut blight spray app"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "cliflo_user", 
                      label = "Cliflo account username"), 
            passwordInput(inputId = "cliflo_pass", 
                          label = "Cliflo account password"),
            dateRangeInput(inputId = "date_range", 
                           label = "Climate period",
                           start = "2021-09-01"),
            numericInput(inputId = "agent_no",
                         label = "NIWA weather station number",
                         value = 17603),
            actionButton(inputId = "download_climate",
                         label = "Download weather data", class = "btn-info"),
            hr(),
            h3("Spray application"),
            dateInput(inputId = "spray_day",
                      label = "Date of application"),
            textInput(inputId = "spray_time", 
                      label = "Time of application", 
                      # value = "00", 
                      placeholder = "Please input hours as 00 to 23"),
            actionButton(inputId = "apply_spray",
                         label = "Apply spray", class = "btn-success")
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            # textOutput("check"),
            # dataTableOutput("check_dt"),
           plotOutput("No_spray"),
           plotOutput("Post_spray")
        )
    )
)

# Define server logic required to draw a histogram ----------
server <- function(input, output) {
    # add NIWA STATION 
    user <- reactive({ input$cliflo_user })
    pass <- reactive({ input$cliflo_pass })
    # input dates
    date_range <- reactive({ input$date_range })
    # agent_no
    agent_no <- reactive({ input$agent_no })
    ## Checking input formats
    download_weather <- eventReactive(input$download_climate,
                                      {
                                          paste(user(), pass(), date_range()[1],
                                                 date_range()[2])
                                      })
    # Climate data
    climate <- eventReactive(input$download_climate,{
        me <- cf_user(username = user(), password = pass())
        my.dts <- cf_datatype(c( 3, 4), #rainfall, temperature
                              c( 1, 2),
                              list( 2, 4),
                              c(NA,NA))
        # Broadfield weather station
        my.stations <- cf_station(agent_no())
        cf.datalist <- cf_query(user = me,
                                datatype = my.dts,
                                station = my.stations,
                                start_date = paste(date_range()[1], "00"),
                                end_date = paste(date_range()[2], "00"))
        
        Rain <- as.data.table(cf.datalist[[1]])
        Temp <- as.data.table(cf.datalist[[2]])
        # station information feedback --------------------------------------------
        rain_renamed <- Rain[, .(Time = `Date(local)`, RainHour = `Amount(mm)`)]
        temp_renamed <- Temp[, .(Time = `Date(local)`, OutTemp = `Tmean(C)`, 
                                 OutHumi = `RHmean(%)`)]
        DT <- merge.data.table(temp_renamed, rain_renamed, by = "Time")
        DT
        
    })
 
    
    
### Render output 
    output$check <- renderText({
        download_weather()
    })
    output$check_dt <- renderDataTable({climate()})
    output$No_spray <- renderPlot({
        # generate bins based on input$bins from ui.R
       

        # draw the histogram with the specified number of bins
    })
    output$Post_spray <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        
        # draw the histogram with the specified number of bins
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
