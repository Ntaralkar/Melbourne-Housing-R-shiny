#importing libraries
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
require(plotly)
library(leaflet)
library(DT)
library(markdown)
library(sandwich)

house = read.csv("Melbourne_housing_FULL.csv",header = T,stringsAsFactors = F)

library(shiny)


ui <- navbarPage("Melbourne Housing!",
                 
                 tabPanel("Introduction", h3("INTRODUCTION"),br(), h4("Melbourne is the second most populous city in Australia and it is ranked the world's most liveable city. The housing market in Melbourne is 
                                                                      changing rapidly, before you think of moving to Melbourne, you should see this!"), 
                          br(), h4("In this webpage, I have created visualizations that provide different kinds of information about the housing market in Melbourne. The data for these visualizations were extracted from the dataset melbourne_housing_data, which contains 17.408 randomly sampled and geocoded house sales for the last 6 months in Melbourne."),
                          br(), h4("To see the first visualization scroll down or click on the next tab below")),
                 tabPanel("Overview" ,sidebarLayout(sidebarPanel(
                   
                   selectInput("Rooms", "Rooms", choices = sort(unique(house$Rooms)), selectize = TRUE),
                   selectInput("Car", "Car Parking", choices = sort(unique(house$Car)), selectize = TRUE),
                   selectInput("Bathroom", "Bathroom", choices = sort(unique(house$Bathroom)), selectize = TRUE),
                   sliderInput("sliderDistance", "Distance from CBD", value = range(house$Distance), min = min(house$Distance), max = max(house$Distance)),
                   sliderInput("pslider",h3("Price"), round = TRUE, value = range(house$Price), min = min(house$Price), max = max(house$Price)) ,
                   radioButtons("dist1", "House type:",c("HOUSE, COTTAGE, VILLA, TERRACE" = "h",
                                                         "UNIT, DUPLEX" = "u",
                                                         "TOWN HOUSE" = "t"))), 
                   mainPanel(leafletOutput("overview1"))))
                 
                 )


server <- function(session, input, output){
  
  
  
  output$overview1 <- renderLeaflet(
    
    
    
    { 
      
      icons <- awesomeIcons(
        icon = 'ios-close',
        iconColor = 'black',
        library = 'ion',
        #markerColor = pal[1:length(levels(as.factor(unique(df$Flowering_Time))))]
        if (input$dist1 == "h") {markerColor = "blue"}
        else if (input$dist1 == "u") {markerColor = "orange"}
        else if (input$dist1 == "t") {markerColor = "red"}
        
        
      )
      
      
      #house <- house %>% filter(Rooms %in% input$Rooms)
      house <- house %>% filter(Type %in% input$dist1 & Car %in% input$Car & Rooms %in% input$Rooms & Bathroom %in% input$Bathroom & Price > input$pslider[1] & Price < input$pslider[2] & Distance > input$sliderDistance[1] & Distance < input$sliderDistance[2])
      #house <- house %>% filter(Bathroom %in% input$Bathroom)
      
      
      #house <- house %>% filter(Price > input$pslider[1] & Price < input$pslider[2])
      #house <- house %>% filter(Distance > input$sliderDistance[1] & Distance < input$sliderDistance[2])
      #house <- house %>% filter(Type %in% input$dist1)
      
      
      #center_lon = median(house$Longtitude)
      #center_lat = median(house$Lattitude)
      
      
      
      
      m <-leaflet() %>% addTiles() %>%
        addAwesomeMarkers(lng = ~Longtitude, lat = ~Lattitude, data=house,icon=icons, label = house$Suburb,
                          popup = paste("Price : ", as.character(house$Price), "<br>",
                                        "Suburb : ", as.character(house$Suburb), "<br>",
                                        "Cars : ", as.character(house$Car),"<br>",
                                        "Bathroom : ", as.character(house$Bathroom), "<br>",
                                        "Rooms : ", as.character(house$Rooms), "<br>",
                                        "Bedrooms : ", as.character(house$Bedroom2),"<br>"
                          ))
      # controls
      #setView(lng=center_lon, lat=center_lat, zoom=12)
      
      m
      
      
    })
  
  
  
  
}

shinyApp(ui, server)


if(house$Rooms == "2")
{
  return(TRUE)
}
