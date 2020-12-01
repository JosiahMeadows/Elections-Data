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
library(janitor)
library(readxl)

combined <- readRDS("combined.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage(
        "Elections Data",
        
        tabPanel("About",
                 # Application title
                 titlePanel("Voter Registration in the United States"),
                 br(),
                 h3("About this Project"),
                 p("I spent a while looking through a spreadsheet of datasets my TF shared with me, and I found a category on voter registration. Ever since starting Gov 50, I knew I wanted to do something with elections, so this category made sense. I wanted to display data registration levels by month, year, and by party affiliation. I thought it would be interesting to hone in on a swing state like Florida and see whether more Democrats are registering than Republicans or vice versa. I also thought it would be interesting to create vertical lines to denote significant events which may have caused an increase in voter registration (e.g. the killing of George Floyd). The Florida Department of State Website offers datasets with this information. I created a visualization in Shiny App. I need to figure out how to rearrange the months in the x-axis. Instead of displaying all of the states in one graph, I intend to create an input section where the user can choose a state and see the data."),
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
                 titlePanel("Voter Registration"),
                 br(),
                 h2("About the Data"),
                 p("The data does not display any significant change in voter registration across the time period"),
        
                 
                 plotOutput("registration_numbers"),
                 
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$registration_numbers <- renderPlot({
        
        fl_dec <- read_xlsx("party-affilation-by-county-09-2020.xlsx",
                            skip = 3) %>% 
            clean_names() %>%
            drop_na() %>%
            filter(county == "TOTALS")
        
        
        fl_jan <- read_xlsx("party-affilation-by-county-09-2020.xlsx", 
                            skip = 3, sheet = 2) %>% 
            clean_names() %>% 
            drop_na() %>% 
            filter(county == "TOTALS")
        
        fl_feb <- read_xlsx("party-affilation-by-county-09-2020.xlsx", 
                            skip = 3, sheet = 3) %>% 
            clean_names() %>% 
            drop_na() %>% 
            filter(county == "TOTALS")
        
        fl_mar <- read_xlsx("party-affilation-by-county-09-2020.xlsx", 
                            skip = 3, sheet = 4) %>% 
            clean_names() %>% 
            drop_na() %>% 
            filter(county == "TOTALS")
        
        fl_apr <- read_xlsx("party-affilation-by-county-09-2020.xlsx", 
                            skip = 3, sheet = 5) %>% 
            clean_names() %>% 
            drop_na() %>% 
            filter(county == "TOTALS")
        
        fl_may <- read_xlsx("party-affilation-by-county-09-2020.xlsx", 
                            skip = 3, sheet = 6) %>% 
            clean_names() %>% 
            drop_na() %>% 
            filter(county == "TOTALS")
        
        fl_jun <- read_xlsx("party-affilation-by-county-09-2020.xlsx", 
                            skip = 3, sheet = 7) %>% 
            clean_names() %>% 
            drop_na() %>% 
            filter(county == "TOTALS")
        
        fl_jul <- read_xlsx("party-affilation-by-county-09-2020.xlsx", 
                            skip = 3, sheet = 8) %>% 
            clean_names() %>% 
            drop_na() %>% 
            filter(county == "TOTALS")
        
        fl_aug <- read_xlsx("party-affilation-by-county-09-2020.xlsx", 
                            skip = 3, sheet = 9) %>% 
            clean_names() %>% 
            drop_na() %>% 
            filter(county == "TOTALS")
        
        combined <- bind_rows(fl_dec,
                              fl_jan,
                              fl_feb,
                              fl_mar,
                              fl_apr,
                              fl_may,
                              fl_jun,
                              fl_jul,
                              fl_aug) %>%
            
            mutate(month = c("Dec",
                             "Jan",
                             "Feb",
                             "Mar",
                             "Apr",
                             "May",
                             "Jun",
                             "Jul",
                             "Aug")) %>%
            
            mutate(month = fct_relevel(month, "Dec", "Jan", "Feb", "Mar", "Apr",
                                       "May", "Jun", "Jul", "Aug")) %>%
            
            pivot_longer(cols = republican_party_of_florida:florida_democratic_party,
                         names_to = "party") %>% 
            
            ggplot(mapping = aes(x = month, y = value, color = party, group = party)) +
            geom_point() +
            geom_line() +
            labs(title = "Change in Florida Registration by Month (2019-2020)",
                 x = "Month",
                 y = "Number of Registered Voters",
                 caption = "Florida Department of State Website") +
            theme(legend.position = "right", 
                  text = element_text(family = "Palatino"),
                  axis.text.x = element_text(size = 11),
                  axis.text.y = element_text(size = 10)) +
            scale_color_manual(values = c("blue", "red")) +
            scale_y_continuous(labels = scales::label_number()) +
            geom_vline(xintercept = "Mar", color = "gray3", linetype = "longdash") +
            geom_vline(xintercept = "Jun", color = "gray3", linetype = "longdash") +
            geom_text(aes(x = "Jun", y = 5090000, label = "Killing of \n George Floyd"),
                      color = "black") +
            geom_text(aes(x = "Mar", y = 5000000, label = "Pandemic \n Accelerates"),
                      color = "black")
        
        combined

    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
