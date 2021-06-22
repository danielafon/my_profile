#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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


############## SERVER ##############  
server <- function(input, output) {
    # Tab 1: Demo dataset
    output$demo <- renderTable({
        head(listings_, n = input$obs)
    })
    
    # Tab 5: Map Boston
    output$map_bdx <- renderLeaflet({
        leaflet(data=listing_map) %>%
            addTiles() %>% 
            setView(lng = -71.05, lat = 42.36, zoom = 12) %>% 
            addMarkers(lng=~longitude, lat=~latitude,
                       popup = ~paste(
                           "<b>", neighbourhood, "</b><br/>",
                           "Type: ", room_type, "<br/>",
                           "count: ", as.character(nb_bnb), "<br/>",
                           "price: ", round(price), "<br/>",
                           "nb_reviews: ", round(nb_reviews), "<br/>",
                           "available_per_year: ", round(availability_365), "<br/>"
                       ), 
                       clusterOptions = markerClusterOptions())
    })
    
    output$room_type_bdx <- renderPlot({
        listings_ %>% 
            ggplot()+
            geom_bar(aes(x=room_type), fill='sienna1')+
            theme_minimal()
    })
    
    
    
    
    # Tab 6: Map des zones
    listing_zone <- reactive({
        listings_ %>% 
            filter(neighbourhood == input$zone)
    })
    
    output$map <- renderLeaflet({
        leaflet(data=listing_zone()) %>%
            addTiles() %>% 
            setView(lng = median(listing_zone()$longitude), lat = median(listing_zone()$latitude), zoom = 15) %>% 
            addMarkers(lng=~longitude, lat=~latitude,
                       popup = ~paste(
                           "<b>", name, "</b><br/>",
                           "type: ", room_type, "<br/>",
                           "price: ", round(price), "<br/>",
                           "minimum_nights: ", round(minimum_nights), "<br/>",
                           "available_per_year: ", round(availability_365), "<br/>",
                           "income_monthly: ", round(income_monthly), "<br/>"
                       ),
                       clusterOptions = markerClusterOptions())
    })
    
    output$room_type <- renderPlot({
        listing_zone() %>% 
            ggplot()+
            geom_bar(aes(x=room_type), fill='sienna1')+
            theme_minimal()
    })
    
    output$nb_bnb <- renderText({
        round(nrow(listing_zone()))
    })
    
    output$price <- renderText({
        round(mean(listing_zone()$price, na.rm = T))
    })
    
    output$available <- renderText({
        round(mean(listing_zone()$availability_365, na.rm = T))
    })
    
    output$night <- renderText({
        round(mean(listing_zone()$minimum_nights, na.rm = T))
    })
    
    output$rate <- renderText({
        round(mean(listing_zone()$number_of_reviews, na.rm = T))
    })
    
    output$income <- renderText({
        round(mean(listing_zone()$income_monthly, na.rm = T))
    })
}
