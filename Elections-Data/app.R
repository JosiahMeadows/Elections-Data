#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# First, load all necessary libraries here.

library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(readxl)
library(dplyr)
library(leaflet)
library(tigris)
library(ggthemes)
library(gtsummary)
library(broom.mixed)
library(gt)

# There is where you put the RDS, which makes sure that the Shiny App knows the
#information from the .Rmd file.

combined_fl <- readRDS("combined_fl.rds")
combined_2 <- readRDS("combined_2.rds")
covid <- readRDS("covid.RDS")
combined_intermediate <- readRDS("combined_intermediate.RDS")
combined_full <- readRDS("combined_full.RDS")
table <- readRDS("table.rds")
jobs <- readRDS("jobs.RDS")

# The UI Displays everything that we want to show.

ui <- fluidPage(theme = shinytheme("flatly"),
    navbarPage(
        "Voter Registrations",
        
        tabPanel("Data Visualization",
                 
                 # This is where I put the title for my first panel.
                 
                 titlePanel("Forces Behind Florida Voter Registrations"),
                 
                 br(),
                 
                 h2("Linear Regressions"),
                 
                 p("First, I created two linear regressions showing voter
                   registrations over time. I then added vertical lines on the
                   x-axis denoting the occurrence of significant events in 2020.
                   The goal was to see if there was any noticeable change in
                   registrations immediately after those events took place.
                   The first graph breaks down the Florida registration numbers
                   by party, and the second combines all of the parties."),
                 
                 br(),
                 
                 p("According to both of the regressions below, there does not
                   seem to be any significant change in voter registration
                   levels immediately after the events indicated by the vertical
                   lines. It does appear, as one would expect, that
                   registrations slowed as the pandemic accelerated. Voter 
                   registrations did increase after the killing of George Floyd,
                   but they were already trending upwards in May before his
                   death."),
                 
                 br(),
                 
                 # These functions plot the first two graphs in the Data 
                 # Visualization panel!
                 
                 plotOutput("registration_numbers"),
                 
                 br(),
                 
                 plotOutput("registration_numbers2"),
                 
                 h2("Interactive Maps"),
                 p("Second, I created three interactive maps to see if there 
                   were any striking visual similarities or differences between
                   the datasets I used. The user can select the month and
                   visually compare rising unemployment and COVID-19 cases with
                   increasing voter registrations across different Florida
                   counties."),
                 
                 br(),
                 
                 h3("Total Voter Registrations by Month in Florida"),
                 h4("(Hover over the map to see the county name and voter
                    registration numbers.)"),
                 
                 br(),
                 
                 # Here, I create the drop down list. I make sure it shows the
                 # full names of each month--not just lowercase abbreviations.
                 
                 selectInput("A", "Choose month:",
                             c("January" = "jan",
                               "February" = "feb",
                               "March" = "mar",
                               "April" = "apr",
                               "May" = "may",
                               "June" = "jun",
                               "July" = "jul",
                               "August" = "aug")),
                 
                 # This is the code for the second drop down list, which allows
                 # the user to choose a party and see the relevant data. I 
                 # reformat the names of the parties here.
                 
                 selectInput("A2", "Choose party:",
                             c("Democrat" = "florida_democratic_party",
                               "Republican" = "republican_party_of_florida",
                               "Minor Party" = "minor_party",
                               "No Party Affiliation" = "no_party_affiliation",
                               "Totals" = "totals")),
                 
                 #Here is an alternate way of doing the same step:
      
                 #selectInput("A2", "Choose party:", combined_full$party,
                             #selected = "florida_democratic_party"),
                 
                 # This Leaflet is the map that I display below. This command
                 # prints the beautiful map. Exciting!
                 
                 leafletOutput("registration_numbers3"),
                 
                 # More titles and subtitles here.
                 
                 br(),
                 
                 h3("Total COVID-19 Cases in Florida by Month"),
                 h4("(Hover over the map to see the county name and number of 
                    cases and deaths.)"),
                 
                 br(),
                 
                 selectInput("B", "Choose month:",
                             c("January" = 1,
                               "February" = 2,
                               "March" = 3,
                               "April" = 4,
                               "May" = 5,
                               "June" = 6,
                               "July" = 7,
                               "August" = 8,
                               "September" = 9,
                               "October" = 10,
                               "November" = 11)),
                 leafletOutput("covid_cases"),
                 
                 br(),
                 
                 h3("Total Unemployment Numbers in Florida by Month"),
                 h4("(Hover over the map to see the county name and number of 
                    unemployment and employment numbers.)"),
                 
                 br(),
                 
                 selectInput("C", "Choose month:",
                             c("January" = 1,
                               "February" = 2,
                               "March" = 3,
                               "April" = 4,
                               "May" = 5,
                               "June" = 6,
                               "July" = 7,
                               "August" = 8,
                               "September" = 9,
                               "October" = 10)),
                 leafletOutput("job_numbers")
                 
                 
        ),
        
        tabPanel("Model",

                 h3("Statistical Model"),
                 
                 p("I created a statistical model to measure the relationship
                   between unemployment levels, COVID-19 cases and deaths, and
                   voter registration numbers. The table below displays the 
                   results. My hypothesis was that the more a particular region
                   suffered with respect to health and the economy, the more
                   people in that region would register to vote. My reasoning
                   was that when circumstances grow dire, political leadership
                   gains even more importance and the cost of not participating
                   in an election rises."),
                 
                 br(),
                 
                 p("While the findings in the table below are not necessarily
                   definitive, they are consistent with the hypothesis. The
                   intercept (27,749) represents the average number of FL voter
                   registrations in 2020. There appears to be very little 
                   relationship between the number of cases and registration
                   numbers (-0.06). However, there is a more significant 
                   correlation between unemployment numbers and registrations
                   (3.3). The model suggests that the higher unemployment, the
                   more voter registrations there will be. Deaths from COVID-19
                   are also positively correlated with voter registration 
                   numbers. While the findings are not definitive, they are
                   consistent with the hypothesis that hardship with respect to
                   health and the economy increases the cost of staying out of 
                   the electoral process. (Whether those who registered actually
                   voted is outside the scope of this project.)"),
                 
                 br(),
                 
                 gt_output("table"),
                 
                 ),
        
        tabPanel("About",

                 h3("About the Project"),
                 
                 p("The 2020 U.S. Presidential Election saw the highest voter
                   turnout in over a century. A devastating pandemic, economic
                   pain, civil unrest, and movements for racial justice have
                   often been cited as the leading factors which mobilized the
                   masses. The goal of this project is to measure how closely
                   rising unemployment, COVID-19 deaths and cases, and other
                   pivotal events correlate with increases in FL voter
                   registration numbers. By quantifying the relationship, one
                   can gain insights as to which variables were the strongest
                   predictors of voter registrations. Although my model is 
                   predictive rather than causal, it still could potentially
                   allow one to better understand the forces which impel people 
                   to take the first step in getting involved in the electoral
                   process."),
                 
                 h3("About the Data"),
                 p("I decided to hone in on the State of Florida, a critical 
                   battleground state which has long been considered a
                   microcosm of U.S. politics. I gathered the data on
                   registration numbers from the official",
                   a("Florida Department of State",
                     href = "https://dos.myflorida.com/elections/data-statistics/voter-registration-statistics/voter-registration-reportsxlsx/"),
                   "website. For COVID-19 cases and deaths in Florida, I
                   obtained data from",
                   a("The New York Times",
                     href = "https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html"),
                   "based on reports from state and local health agencies. The
                   data on employment comes from the",
                   a("Florida Department of Economic Opportunity.",
                     href = "https://floridajobs.org/workforce-statistics/data-center/statistical-programs/local-area-unemployment-statistics"),
                   "I cleaned and organized all datasets for the purposes of
                   this project."),
                 
                 h3("About Me"),
                 
                 p("My name is Josiah Meadows, and I am a sophomore at Harvard
                 University pursuing a B.A. in Government with a secondary in
                Economics. Contact me at josiahmeadows@college.harvard.edu. The
                   source code on my Github can be found",
                   a("here.",
                     href = "https://github.com/JosiahMeadows/Elections-Data"),
                   "My LinkedIn profile can be found",
                   a("here.",
                     href = "https://www.linkedin.com/in/josiah-meadows-1441601b5/")),


        )
        

        
    )
)

# Define server logic required to plot the graphs and maps
# This section of the Shiny App -- the server -- actually executes everything.


server <- function(input, output) {
    
    output$registration_numbers <- renderPlot({
        combined_fl %>% 
            
            # Here, I tell it to create the first plot.
            
            ggplot(mapping = aes(x = month, y = value, color = party, group = party)) +
            geom_line(lwd = 1) +
            labs(title = "Monthly Change in Florida Registration by Party (2019-2020)",
                 x = "Month",
                 y = "Total Number of Registered Voters",
                 caption = "Source: Florida Department of State (dos.myflorida.com)") +
            theme_classic() +
            theme(legend.position = "right",
                  text = element_text(family = "Helvetica Neue"),
                  axis.text.x = element_text(size = 10),
                  axis.text.y = element_text(size = 10),
                  plot.title = element_text(size = 16, face = "bold"),
                  plot.caption = element_text(face = "italic")) +
            scale_color_manual(values = c("dodgerblue", "lightcoral"),
                               name = "Party",
                               labels = c("Democratic", "Republican")) +
            scale_y_continuous(labels = scales::comma) +
            
            # Create the vertical lines!
            
            geom_vline(xintercept = "Mar.", color = "gray") +
            geom_vline(xintercept = "June", color = "gray") +
            
            # Add the text to describe the lines!
            
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
    
    # Now let's go ahead and create the second plot!
    
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
                  text = element_text(family = "Helvetica Neue"),
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
 
# Now this is the really cool part where I add the maps. Leaflet allows us to
# to do that!
       
    output$registration_numbers3 <- renderLeaflet({
 
      county_shapes <- counties(state = "12", cb = TRUE)
    
      sb_county <- combined_full %>% 
        filter(month == input$A) %>%
        filter(party == input$A2) %>% 
        clean_names() %>% 
        select(county, totals = value)
      
      counties_merged_sb <- geo_join(county_shapes, sb_county, "NAME", "county")
      
      # This is where I specify the specific shades of greens and the numbers
      # to which they correspond.
      
      pal <- colorBin("Greens",
                      bins = c(0, 100, 500, 1000, 5000, 10000, 100000, 200000, Inf))
      
      # This creates the nice popup feature which gives the user the
      # information.

      popup_sb <- paste("County: ", as.character(counties_merged_sb$NAME),
                         "Total Registrations: ",
                        as.character(counties_merged_sb$totals))
      
      # Finally, print the map. The code below also highlights individual
      # counties, a nice enhancement to the interactive experience!
      
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(-82, 28, zoom = 6) %>% 
        addPolygons(data = counties_merged_sb,
                    fillColor = ~pal(counties_merged_sb$totals),
                    fillOpacity = 0.7,
                    weight = 0.2,
                    color = "#666",
                    smoothFactor = 0.2,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = popup_sb,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal",
                                   padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>% 
        addLegend(pal = pal,
                  values = counties_merged_sb$total,
                  position = "bottomright",
                  title = "Total Registrations")
      
    })
    
    # This is where I create the second map, following the same rules as the 
    # first. I simply tailor a few things now that we're dealing with Covid-19
    # data instead of voter registrations. (Red instead of Green, etc).
    
    output$covid_cases <- renderLeaflet({
      
      county_shapes <- counties(state = "12", cb = TRUE)
      
      sb_county2 <- covid %>% 
        clean_names() %>%
        drop_na() %>% 
        filter(month == input$B)
      
      counties_merged_sb2 <- geo_join(county_shapes, sb_county2, "NAME", "county")
      
      pal <- colorBin("Reds", bins = c(0, 100, 500, 1000, 5000, 10000, 100000, 200000, Inf))
      
      popup_sb2 <- paste("County: ", as.character(counties_merged_sb2$NAME),
                         "New Cases: ", as.character(counties_merged_sb2$total_cases),
                         "New Deaths: ", as.character(counties_merged_sb2$total_deaths))
      
      # Create the map!
      
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(-82, 28, zoom = 6) %>% 
        addPolygons(data = counties_merged_sb2,
                    fillColor = ~pal(counties_merged_sb2$total_cases),
                    fillOpacity = 0.7,
                    weight = 0.2,
                    color = "#666",
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
    
    output$job_numbers <- renderLeaflet({
      
      county_shapes <- counties(state = "12", cb = TRUE)
      
      sb_county3 <- jobs %>% 
        clean_names() %>%
        drop_na() %>% 
        filter(month == input$C)
      
      counties_merged_sb3 <- geo_join(county_shapes, sb_county3, "NAME", "county")
      
      pal <- colorBin("Oranges",
                      bins = c(0, 100, 500, 1000, 5000, 10000, 100000, 200000, Inf))
      
      popup_sb3 <- paste("County: ", as.character(counties_merged_sb3$NAME),
                          "Total Unemployment: ",
                          as.character(counties_merged_sb3$total_unemp),
                          "Total Employment: ", 
                          as.character(counties_merged_sb3$total_emp))
      
      # Create the map!
      
      leaflet() %>% 
        addProviderTiles("CartoDB.Positron") %>% 
        setView(-82, 28, zoom = 6) %>% 
        addPolygons(data = counties_merged_sb3,
                    fillColor = ~pal(counties_merged_sb3$total_unemp),
                    fillOpacity = 0.7,
                    weight = 0.2,
                    color = "#666",
                    smoothFactor = 0.2,
                    highlight = highlightOptions(
                      weight = 5,
                      color = "#666",
                      fillOpacity = 0.7,
                      bringToFront = TRUE),
                    label = popup_sb3,
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")) %>% 
        addLegend(pal = pal,
                  values = counties_merged_sb3$total_unemp,
                  position = "bottomright",
                  title = "Total Unemployment")
      
    })

    output$table <- render_gt({
      
      table %>% 
        as_gt() %>% 
        tab_header(title = md("*Regression of Unemployment Levels, COVID Cases & Deaths on FL Voter Registrations*"))
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
