library(tidyverse)
library(ggplot2)
library(shiny)
library(leaflet)
library(leaflet.extras)

ui <- shinyUI(fluidPage(
  titlePanel("Micronesia Island State Analysis"),
  
  navbarPage(
    title = "", 
    tabPanel("Introduction", 
             # 这里放入介绍内容
    ),
    tabPanel("General Description",
             tabsetPanel(
               tabPanel("Map of the Island State", 
                        selectInput("stateSelect", "Select State", choices = c("Pohnpei", "Chuuk", "Kosrae", "Yap")),
                        leafletOutput("selectedStateMap")
               ),
               tabPanel("Map Showing the Location of the Island State in the World", 
                        leafletOutput("worldMap")
               ),
               tabPanel("Key Facts about the Island State"),
               tabPanel("A Brief Narrative Description of the Island State")
             )
    ),
    tabPanel("Key Demographics"),
    tabPanel("Comparison with Other Regional Island States"),
    tabPanel("SWOT Analysis"),
    tabPanel("Reference")
  )
))


server <- function(input, output) {
  
  # 根据选择的状态显示相应的地图
  output$selectedStateMap <- renderLeaflet({
    switch(input$stateSelect,
           "Pohnpei" = {
             # Set the coordinates
             micronesia_Pohnpei <- c(6.8874, 158.2150)
             
             # Create map
             micronesia_Pohnpei_map <- leaflet() |>
               # Add base layer
               addProviderTiles(providers$OpenStreetMap) |> 
               # Set the view and zoom level
               setView(lng = micronesia_Pohnpei[2], lat = micronesia_Pohnpei[1], zoom = 11) |>
               # Add a marker with a popup
               addMarkers(lng = micronesia_Pohnpei[2], lat = micronesia_Pohnpei[1], 
                          popup = "State: Pohnpei<br>State Capital: Kolonia<br>Administrative Capital: Palikir") |> 
               # Add a label marker
               addLabelOnlyMarkers(lng = micronesia_Pohnpei[2], lat = micronesia_Pohnpei[1],
                                   label = "State: Pohnpei, Administrative Capital: Palikir",
                                   labelOptions = labelOptions(noHide = TRUE,
                                                               direction = 'auto',
                                                               offset = c(-100, -10)))
             
             micronesia_Pohnpei_map
           },
             "Chuuk" = {
               # Set the coordinates
               micronesia_Chuuk <- c(7.4500, 151.8500)
               
               # Create map
               micronesia_Chuuk_map <- leaflet() |>
                 # Add base layer
                 addProviderTiles(providers$OpenStreetMap) |> 
                 # Set the view and zoom level
                 setView(lng = micronesia_Chuuk[2], lat = micronesia_Chuuk[1], zoom = 11) |>
                 # Add a marker with a popup
                 addMarkers(lng = micronesia_Chuuk[2], lat = micronesia_Chuuk[1], 
                            popup = "State: Chuuk<br>Capital: Weno") |> 
                 # Add a label marker
                 addLabelOnlyMarkers(lng = micronesia_Chuuk[2], lat = micronesia_Chuuk[1],
                                     label = "State: Chuuk, Capital: Weno",
                                     labelOptions = labelOptions(noHide = TRUE,
                                                                 direction = 'auto',
                                                                 offset = c(-20, 0)))
               
               micronesia_Chuuk_map
             },
               "Kosrae" = {
                 # Set the coordinates
                 micronesia_Kosrae <- c(5.3258, 163.0086)
                 
                 # Create map
                 micronesia_Kosrae_map <- leaflet() |>
                   # Add base layer
                   addProviderTiles(providers$OpenStreetMap) |> 
                   # Set the view and zoom level
                   setView(lng = micronesia_Kosrae[2], lat = micronesia_Kosrae[1], zoom = 11) |>
                   # Add a marker with a popup
                   addMarkers(lng = micronesia_Kosrae[2], lat = micronesia_Kosrae[1], 
                              popup = "State: Kosrae<br>Capital: Tofol") |> 
                   # Add a label marker
                   addLabelOnlyMarkers(lng = micronesia_Kosrae[2], lat = micronesia_Kosrae[1],
                                       label = "State: Kosrae, Capital: Tofol",
                                       labelOptions = labelOptions(noHide = TRUE,
                                                                   direction = 'right',
                                                                   offset = c(40, -10)))
                 
                 micronesia_Kosrae_map
               },
                 "Yap" = {
                   # Set the coordinates
                   micronesia_Yap <- c(9.5167, 138.1333)
                   
                   # Create map
                   micronesia_Yap_map <- leaflet() |>
                     # Add base layer
                     addProviderTiles(providers$OpenStreetMap) |> 
                     # Set the view and zoom level
                     setView(lng = micronesia_Yap[2], lat = micronesia_Yap[1], zoom = 11) |>
                     # Add a marker with a popup
                     addMarkers(lng = micronesia_Yap[2], lat = micronesia_Yap[1], 
                                popup = "State: Yap<br>Capital: Colonia") |> 
                     # Add a label marker
                     addLabelOnlyMarkers(lng = micronesia_Yap[2], lat = micronesia_Yap[1],
                                         label = "State: Yap, Capital: Colonia",
                                         labelOptions = labelOptions(noHide = TRUE,
                                                                     direction = 'auto',
                                                                     offset = c(-90, -10)))
                   
                   micronesia_Yap_map
                 }
    )
                 })
  
  output$worldMap <- renderLeaflet({
    # Set the coordinates
    micronesia_Pohnpei <- c(6.8874, 158.2150)
    micronesia_Chuuk <- c(7.4500, 151.8500)
    micronesia_Kosrae <- c(5.3258, 163.0086)
    micronesia_Yap <- c(9.5167, 138.1333)
    
    # Create map
    micronesia_map_world <- leaflet() |> 
      addProviderTiles(providers$Esri.NatGeoWorldMap) |> 
      setView(lng = micronesia_Chuuk[2], lat = micronesia_Chuuk[1], zoom = 3) |> 
      addCircles(lng = micronesia_Pohnpei[2], lat = micronesia_Pohnpei[1],
                 radius = 650000, color = '#D90429', fillColor = '#D90429',
                 fillOpacity = 0.7, weight = 2, dashArray = '5, 5',
                 popup = "Pohnpei") |> 
      addCircles(lng = micronesia_Chuuk[2], lat = micronesia_Chuuk[1],
                 radius = 550000, color = '#134074', fillColor = '#134074',
                 fillOpacity = 0.7, weight = 2, dashArray = '5, 5',
                 popup = "Chuuk") |> 
      addCircles(lng = micronesia_Kosrae[2], lat = micronesia_Kosrae[1],
                 radius = 450000, color = '#386641', fillColor = '#386641',
                 fillOpacity = 0.7, weight = 2, dashArray = '5, 5',
                 popup = "Kosrae") |> 
      addCircles(lng = micronesia_Yap[2], lat = micronesia_Yap[1],
                 radius = 500000, color = '#F9C80E', fillColor = '#F9C80E',
                 fillOpacity = 0.7, weight = 2, dashArray = '5, 5',
                 popup = "Yap")
    
    micronesia_map_world
  })
  
  # 其他标签页的逻辑可以在这里添加
  
               }

shinyApp(ui = ui, server = server)
               