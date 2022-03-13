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
        # tidy up 
        rain_renamed <- Rain[, .(Time = `Date(local)`, RainHour = `Amount(mm)`)]
        temp_renamed <- Temp[, .(Time = `Date(local)`, OutTemp = `Tmean(C)`, 
                                 OutHumi = `RHmean(%)`)]
        # merge rain and temperature
        DT <- merge.data.table(temp_renamed, rain_renamed, by = "Time")
        DT <- DT[, Day := as.Date(Time, tz = "NZ")
                 ][, RainDay := cumsum(RainHour), by = .(Day)]
        # Fill NAs
        setnafill(DT, type = "locf")
        DT
        
    })
    # calculate 
    scores <- reactive({
        DT <- climate()
        DT[, Multiplier := ifelse(OutHumi<85, 0.995,
                                  1+((5^(OutTemp/20))/100))]
        DT[, Score := 1]
        for (i in 2:nrow(DT)){
            DT$Score[i] <- ifelse(DT$Score[i-1] * DT$Multiplier[i] < 1, 1,
                                  DT$Score[i-1] * DT$Multiplier[i])
            # Started from 8th September 
            # Check spray column 
            # If the spray column = true, reset the score to 1 
            
        }
        DT
    })
    # plot constants

### Render output 
    # output$check <- renderText({
    #     download_weather()
    # })
    # output$check_dt <- renderDataTable({climate()})
    output$No_spray <- renderPlot({
        legendcols <- c("red", "blue", "green")
        names(legendcols) <-  c("Score", "OutTemp", "RainDay")
        
        scores() %>% 
            ggplot(aes(Time, Score)) +
            geom_line(aes(color= "Score"), size = 1.5) +
            geom_smooth(aes(y = OutTemp, color = "OutTemp"), span = 0.05, se = FALSE) +
            geom_line(aes(y = RainDay, color = "RainDay"), size = 1)  +
            scale_color_manual(name = "",
                               values = legendcols,
                               labels = c("Blight Score", "Temperature", "24 Hour Rain"))+
            theme_walnut()
        
    })
    output$Post_spray <- renderPlot({
        # generate bins based on input$bins from ui.R
        
        
        # draw the histogram with the specified number of bins
    })
}


# functions ---------------------------------------------------------------


theme_walnut <- function(){
    theme_linedraw()+
        theme(panel.grid.major.x = element_blank(), 
              panel.grid.minor.x = element_blank(),
              legend.key.width = unit(15, "mm"),
              legend.key = element_rect(colour =  "transparent", fill = "white"))
    
}

# Run the application 
shinyApp(ui = ui, server = server)
