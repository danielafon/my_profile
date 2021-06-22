#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readxl)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(DT)
library(ggplot2)
library(lubridate)
library(leaflet)
library(RColorBrewer)
library(tm)
library(rsconnect)
library(NLP)
library(lubridate)
library(rsconnect)

listings_ <- read_excel("listings .xlsx") 
listings_ <- listings_ %>% 
    mutate(
        income_monthly = round(price*availability_365/12),
        highly_available = availability_365 >=60,
        freq_review = (today() - last_review) <=180
    )
listing_map <- listings_ %>% 
    select(id, neighbourhood, longitude, latitude, room_type, price, number_of_reviews, availability_365, income_monthly) %>% 
    group_by(neighbourhood, room_type) %>% 
    summarise(
        nb_bnb = n(),
        price = mean(price, na.rm = T),
        nb_reviews = mean(number_of_reviews, na.rm = T),
        availability_365 = mean(availability_365, na.rm = T),
        income_monthly = mean(income_monthly, na.rm = T),
        longitude = median(longitude),
        latitude = median(latitude)
    )


col_def <- tibble(
    'neighbourhood_group'="Accommodation area", 
    'room_type'= "Type of housing",
    'highly_available' = 'True/False: the rental is available at least 60 days per year',
    'freq_review' = 'True/False: last review was less than 6 months ago',
    'price'= 'Average pricee', 
    'minimum_nights' = 'Number of nights minimum',
    'number_of_reviews' = "Number of reviews",
    'reviews_per_month' = "Number of reviews per month - average",
    'availability_365' = "Number of days available for rental per year", 
    'income_monthly'= "Revenue per month - average", 
    'calculated_host_listings_count' = 'Number of units per host'
)


############## UI ############## 

main_sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Dashboard', icon = icon('chart-pie'),
                 menuSubItem('Boston', tabName = 'tab_bdx'),
                 menuSubItem('Focus on neighbourhoods', tabName = 'tab_zones')
        ),
        menuItem('Dataset', icon = icon('chart-bar'),
                 menuSubItem('Dataset', tabName = 'tab_demo')
        )
    )
)

main_body <- dashboardBody(
    tabItems( 
        # Page 1
        tabItem( # Tab1: Demo
            tabName = 'tab_demo',
            fluidRow(
                column(3,
                       numericInput("obs", "Number of lines:", 10)
                )),
            div(style = 'overflow-x: scroll',tableOutput("demo"))
        ),
        
        # Page 2
        tabItem( # Tab 1: Boston and surrounding area
            tabName = 'tab_bdx',
            fluidRow(
                valueBox(round(nrow(listings_)), "Airbnb number", icon = icon("airbnb"), color = "red"),
                valueBox(round(mean(listings_$price, na.rm = T)), "Average Price", icon = icon("money-bill-wave"), color = "orange"),
                valueBox(round(mean(listings_$availability_365, na.rm = T)), "Availability per year - average", icon = icon("door-open"), color = "yellow"),
                valueBox(round(mean(listings_$number_of_reviews, na.rm = T)), "Number of reviews - average", icon = icon("star"), color = "red"),
                valueBox(round(mean(listings_$minimum_nights, na.rm = T)), "Minimum number of nights - average", icon = icon("bed"), color = "orange"),
                valueBox(round(mean(listings_$income_monthly, na.rm = T)), "Monthly Revenue - average", icon = icon("credit-card"), color = "yellow")
            ),
            fluidRow(box(leafletOutput("map_bdx"), status = "danger", title = 'Boston', width = 8),
                     box(plotOutput("room_type_bdx"), status = "warning", title = "Distribution of accommodation types", width = 4)
            )
        ),
        tabItem( # Filtre par zones
            tabName = 'tab_zones',
            fluidRow(
                column(3,
                       selectInput("zone",
                                   "Choose the area  :",
                                   choices = unique(listings_$neighbourhood)
                       )
                ),
            ),
            fluidRow(
                valueBox(textOutput('nb_bnb'), "Airbnb number", icon = icon("airbnb"), color = "red"),
                valueBox(textOutput('price'), "Average Price", icon = icon("money-bill-wave"), color = "orange"),
                valueBox(textOutput('available'), "Availability per year - average", icon = icon("door-open"), color = "yellow"),
                valueBox(textOutput('rate'), "Number of reviews - average", icon = icon("star"), color = "red"),
                valueBox(textOutput('night'), "Minimum number of nights - average", icon = icon("bed"), color = "orange"),
                valueBox(textOutput('income'), "Monthly Revenue - average", icon = icon("credit-card"), color = "yellow")
            ),
            fluidRow(box(leafletOutput("map"), status = "danger", title = 'Boston and surrounding area', width = 8),
                     box(plotOutput("room_type"), status = "warning", title = "Distribution of accommodation types", width = 4)
            )
            
        )
    )
)



ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Boston Best Aibnb",
                                    tags$li(class = "dropdown",
                                            tags$a(href="https://www.linkedin.com/in/daniela-fonsecav/", 
                                                   target="_blank", 
                                                   icon("linkedin", "fa-1.5x", lib = "font-awesome"))),
                                    tags$li(class="dropdown",tags$a(href="http://insideairbnb.com/get-the-data.html", icon("database"), "Data source", target="_blank")),
                                    dropdownMenu(
                                        type = "message",
                                        messageItem(
                                            from = "Daniela",
                                            message ="Welcome to my App!",
                                            href = "https://www.linkedin.com/in/daniela-fonsecav/")
                                        ,messageItem(
                                            from = "Gabriel",
                                            message ="Best New York Spots!")
                                    ),
                                    dropdownMenu(type = "notifications",
                                                 notificationItem(
                                                     text = "25 new users today",
                                                     icon("users")
                                                 ),
                                                 notificationItem(
                                                     text = "12 new hosts",
                                                     icon("airbnb"),
                                                     status = "success"
                                                 ),
                                                 notificationItem(
                                                     text = "Server load at 86%",
                                                     icon = icon("exclamation-triangle"),
                                                     status = "warning"
                                                 )
                                    )
                    ), 
                    main_sidebar, 
                    main_body,
                    tags$head(tags$style(HTML(".main-sidebar {background-color: #D6584B !important;}"))
                    )
) 
