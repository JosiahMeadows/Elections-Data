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
library(tidyverse)

police_killings2 <- readRDS("police_killings2.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
    navbarPage(
        "Elections Data",
        
        tabPanel("About",
                 # Application title
                 titlePanel("Police Killings by Region in the United States"),
                 br(),
                 h3("About the Data"),
                 p("I got my data from the fivethirtyeight package, and I joined the police killings dataset with the state info dataset. I was interested in figuring out which region in the United States has the highest number of police killings. According to the data, the South tops all other geographic regions. I created a Data Visualization tab to display this bleak data with a bar grap."),
                 br(),
                 h3("About Me"),
                 br(),
                 h4("Josiah Meadows"),
                 p("I am a sophomore at Harvard University pursuing a B.A. in Government with a secondary in Economics."),
                 p("The source code on Github can be found",
                   a("here.",
                     href = "https://github.com/JosiahMeadows/Elections-Data"))
                 
                 # Sidebar with a slider input for number of bins 
                 
        ),
        
        tabPanel("Data Visualization",
                 # Application title
                 titlePanel("Voters"),
                 br(),
                 h2("About the Data"),
                 p("According to the data, the South has the highest number of police killings in the United States."),
        
                 
                 plotOutput("distPlot2"),
                 
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$distPlot2 <- renderPlot({
        
        police_killings2 %>% 
            group_by(region) %>% 
            summarize(killings = sum(count_by_state)) %>% 
            ggplot(mapping = aes(x = region, y = killings)) +
            geom_col(fill = "turquoise4") +
            labs(title = "Number of Police Killings in the United States",
                 x = "Region",
                 y = "Killings")
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
