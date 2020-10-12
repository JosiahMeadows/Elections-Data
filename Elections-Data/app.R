#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("united"),
    navbarPage(
        "Elections Data",
        tabPanel("Analysis",
                 titlePanel("Elections"),
                 p("This is where I'm going to analyze my data"),
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("bin",
                                     "# of bins:",
                                     min = 50,
                                     max = 100,
                                     value = 30),
                         selectInput(inputId = "state",
                                     label = "Choose a State",
                                     choices = c("OH", "GA", "NY", "MA"))
                        
                         
                     ),
                     mainPanel(
                         plotOutput("distPlot")
                     )
                 )
                 
        ),
        tabPanel("About",
                 # Application title
                 titlePanel("Voters"),
                 br(),
                 h2("About the Data"),
                 p("This is where I will tell you more information about where I got my data."),
                 br(),
                 h2("About Me"),
                 p("I am a sophomore at Harvard University pursuing a BA in Government with a secondary in Economics"),
                 p("The source code on Github can be found",
                   a("here",
                     href = "https://github.com/JosiahMeadows/Elections-Data"))
                 
                 # Sidebar with a slider input for number of bins 
                 
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        binsthing <- seq(min(x), max(x), length.out = input$bin + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = binsthing, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
