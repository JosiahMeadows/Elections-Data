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
library(leaflet)
library(ggthemes)

combined <- readRDS("combined_fl.rds")
covid <- readRDS("covid.RDS")
combined_intermediate <- readRDS("combined_intermediate.RDS")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("lumen"),
    navbarPage(
        "Voter Registrations",
        
        tabPanel("About",
                 # Application title
                 titlePanel("Driving Forces behind Voter Registrations"),
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
                 
                 br(),
                 p("The data does not display any significant change in voter registration across the time period"),
                 
                 br(),
                 plotOutput("registration_numbers"),
                 plotOutput("registration_numbers2"),
                 

                 h3("Total Voter Registrations by Month in Florida"),
                 h4("(Hover over the map to see the county name and voter registration numbers.)"),
                 br(),
                 selectInput("A", "Choose month:",
                             c("January" = "jan", "February" = "feb", "March" = "mar", "April" = "apr", "May" = "may",
                               "June" = "jun", "July" = "jul", "August" = "aug")),
                 selectInput("A2", "Choose party:", combined_intermediate$party, selected = "florida_democratic_party"),
                 leafletOutput("registration_numbers3"),
                 h3("New COVID-19 Cases in Florida by Month"),
                 h4("(Hover over the map to see the county name and number of Covid cases.)"),
                 br(),
                 selectInput("B", "Choose month:",
                             c("January" = 1, "February" = 2, "March" = 3, "April" = 4, "May" = 5,
                               "June" = 6, "July" = 7, "August" = 8, "September" = 9,
                               "October" = 10, "November" = 11, "December" = 12)),
                 leafletOutput("covid_cases"),
        )
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$registration_numbers <- renderPlot({
        combined_fl %>% 
            
            # Create the plot
            
            ggplot(mapping = aes(x = month, y = value, color = party, group = party)) +
            geom_line(lwd = 1) +
            labs(title = "Monthly Change in Florida Registration by Party (2019-2020)",
                 x = "Month",
                 y = "Total Number of Registered Voters",
                 caption = "Source: Florida Department of State (dos.myflorida.com)") +
            theme_classic() +
            theme(legend.position = "right",
                  text = element_text(family = "Palatino"),
                  axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  plot.title = element_text(size = 16, face = "bold"),
                  plot.caption = element_text(face = "italic")) +
            scale_color_manual(values = c("dodgerblue", "lightcoral"),
                               name = "Party",
                               labels = c("Democratic", "Republican")) +
            scale_y_continuous(labels = scales::comma) +
            
            # Create the vertical lines
            
            geom_vline(xintercept = "Mar.", color = "gray") +
            geom_vline(xintercept = "June", color = "gray") +
            
            # Add the text to describe the lines
            
            geom_text(aes(x = "Mar.",
                          y = 5000000,
                          label = "WHO DECLARES\nCOVID-19 PANDEMIC"),
                      color = "gray",
                      size = 3) +
            geom_text(aes(x = "June",
                          y = 5000000,
                          label = "KILLING OF\nGEORGE FLOYD"),
                      color = "gray",
                      size = 3)

    })
    
    output$registration_numbers2 <- renderPlot({
        
        combined_2 %>% 
            
            ggplot(mapping = aes(x = month, y = value, group = party)) +
            geom_line(color = "turquoise3", lwd = 1) +
            labs(title = "Monthly Change in Florida Registration (2019-2020)",
                 x = "Month",
                 y = "Total Number of Registered Voters",
                 caption = "Source: Florida Department of State (dos.myflorida.com)") +
            theme_classic() +
            theme(legend.position = "right",
                  text = element_text(family = "Palatino"),
                  axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  plot.title = element_text(size = 16, face = "bold"),
                  plot.caption = element_text(face = "italic")) +
            scale_y_continuous(labels = scales::comma) +
            
            # Create the vertical lines
            
            geom_vline(xintercept = "Mar.", color = "gray") +
            geom_vline(xintercept = "June", color = "gray") +
            
            # Add the text to describe the lines
            
            geom_text(aes(x = "Mar.",
                          y = 13650000,
                          label = "WHO DECLARES\nCOVID-19 PANDEMIC"),
                      color = "gray",
                      size = 3) +
            geom_text(aes(x = "June",
                          y = 13650000,
                          label = "KILLING OF\nGEORGE FLOYD"),
                      color = "gray",
                      size = 3)
      
    })
    
    output$registration_numbers3 <- renderLeaflet({
 
      county_shapes <- counties(state = "FL", cb = TRUE)
    
      
      sb_county <- combined_intermediate %>% 
        filter(month == input$A) %>%
        filter(party == input$A2) %>% 
        clean_names() %>% 
        select(county, totals = value)
      
      
      counties_merged_sb <- geo_join(county_shapes, sb_county, "NAME", "county")
      
      pal <- colorBin("Greens", bins = c(10, 1000, 5000, 10000, 100000, 200000, Inf))

      popup_sb <- paste0("County: ", as.character(counties_merged_sb$NAME),
                         "\nTotal Registrations: ", as.character(counties_merged_sb$totals))
      
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(-82, 28, zoom = 6) %>% 
        addPolygons(data = counties_merged_sb,
                    fillColor = ~pal(counties_merged_sb$totals),
                    fillOpacity = 0.7,
                    weight = 0.2,
                    smoothFactor = 0.2,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = popup_sb,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>% 
        addLegend(pal = pal,
                  values = counties_merged_sb$total,
                  position = "bottomright",
                  title = "Total Registrations")
      
    })
    
    output$covid_cases <- renderLeaflet({
      
      county_shapes <- counties(state = "FL", cb = TRUE)
      
      sb_county2 <- covid %>% 
        clean_names() %>%
        drop_na() %>% 
        filter(month == input$B)
      
      counties_merged_sb2 <- geo_join(county_shapes, sb_county2, "NAME", "county")
      
      pal <- colorBin("Reds", bins = c(1000, 5000, 10000, 100000, 200000, Inf))
      
      popup_sb2 <- paste0("New Cases: ", as.character(counties_merged_sb2$totals))
      
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(-82, 28, zoom = 6) %>% 
        addPolygons(data = counties_merged_sb2,
                    fillColor = ~pal(counties_merged_sb2$total_cases),
                    fillOpacity = 0.7,
                    weight = 0.2,
                    smoothFactor = 0.2,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = popup_sb2,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>% 
        addLegend(pal = pal,
                  values = counties_merged_sb2$total_cases,
                  position = "bottomright",
                  title = "Total Cases")
      
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server)
