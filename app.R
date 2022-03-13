#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
            submitButton(text = "Download weather data"),
            hr(),
            h3("Spray application"),
            dateInput(inputId = "spray_day",
                      label = "Date of application"),
            textInput(inputId = "spray_time", 
                      label = "Time of application", 
                      # value = "00", 
                      placeholder = "Please input hours as 00 to 23"),
            submitButton()
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram ----------
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
       

        # draw the histogram with the specified number of bins
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
