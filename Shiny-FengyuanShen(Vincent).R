library(tidyverse)
library(ggplot2)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(treemap)

# Read the CSV file and skip the first 4 lines
micronesia_data <- read_csv("data/API_FSM_DS2_en_csv_v2_6235080.csv", skip = 4)

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
               tabPanel("World Map Highlighting the Island State's Location", 
                        leafletOutput("worldMap")
               ),
               tabPanel("Economic Characteristics of the Island State",
                        selectInput("selectedChart", "Choose a Chart", 
                                    choices = c("GDP and GDP Growth", "GDP Per Capita", "Export and Import")),
                        sliderInput("yearRange", "Select Year Range", 
                                    min = 1986, max = 2022, value = c(1986, 2022)),
                        plotOutput("selectedChartPlot")
               ),
               tabPanel("Overview of Political, Social, Cultural, and Environmental Aspects")
             )
    ),
    tabPanel("Key Demographics"),
    tabPanel("Comparison with Other Regional Island States"),
    tabPanel("SWOT Analysis"),
    tabPanel("Reference")
  )
))


server <- function(input, output) {
  
  # The corresponding map is displayed according to the selected state
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
  
  output$selectedChartPlot <- renderPlot({
    
    # A list of selected indicators
    selected_indicators <- c('GDP (current US$)', 'GDP growth (annual %)', 
                             'GDP per capita (current US$)', 
                             'GDP per capita growth (annual %)',
                             'Imports of goods and services (current US$)', 
                             'Exports of goods and services (current US$)', 
                             'Population, total', 'Population growth (annual %)', 
                             'Travel services (% of commercial service exports)')
    
    # Filter data
    filtered_data <- micronesia_data |> 
      filter(`Indicator Name` %in% selected_indicators) |> 
      select(-c(`Country Name`, `Country Code`, `Indicator Code`))
    
    # Convert data to long format
    long_data <- filtered_data |> 
      pivot_longer(cols = `1986`:`2022`, names_to = "Year", values_to = "Value") |> 
      mutate(Year = as.numeric(Year))  # Convert year to numeric
    
    # 根据用户选择的图表类型来选择数据和绘图逻辑
    req(input$selectedChart)  # 确保选择了图表
    selected_data <- long_data |> 
      filter(Year >= input$yearRange[1], Year <= input$yearRange[2])
    
    # 使用 switch 语句来根据选择绘制不同的图表
    switch(input$selectedChart,
           "GDP and GDP Growth" = {
             gdp_growth_data <- long_data |> 
               filter(`Indicator Name` %in% c('GDP (current US$)', 
                                              'GDP growth (annual %)'),
                      Year >= input$yearRange[1], 
                      Year <= input$yearRange[2])
             
             ggplot(data = gdp_growth_data, aes(x = Year)) +
               geom_col(data = filter(gdp_growth_data, `Indicator Name` == 'GDP (current US$)'),
                        aes(y = Value / 10^6, fill = Value), color = "black") +
               scale_fill_gradientn(colours = c("#03045E", "#012A4A", "#013A63", "#023E8A",
                                                "#014F86", "#277DA1", "#577590", "#4D908E", 
                                                "#43AA8B", "#90BE6D", "#F9C74F", "#F9844A",
                                                "#DC2F02", "#D00000"),
                                    values = seq(0, 1, length.out = 14)) +
               # Amplify the GDP growth rate to match the size of GDP
               geom_line(data = filter(gdp_growth_data, `Indicator Name` == 'GDP growth (annual %)'), 
                         aes(y = Value * 10 + 300), color = "#FFC300", linewidth = 1, group = 1) + 
               scale_y_continuous(sec.axis = sec_axis(~./10 - 30, 
                                                      name = "GDP Growth Rate (annual %)")) +
               labs(
                 title = "GDP and GDP Growth Rate Over Time", 
                 x = "Year", y = "GDP (million current US$)"
               ) +
               theme_minimal() +
               theme(
                 plot.title = element_text(hjust = 0.5),
                 axis.text.x = element_text(angle = 45, hjust = 1),
                 legend.position = "none"
               )
           },
           "GDP Per Capita" = {
             gdp_per_capita_data <- long_data |> 
               filter(`Indicator Name` %in% c('GDP per capita (current US$)', 
                                              'GDP per capita growth (annual %)'),
                      Year >= input$yearRange[1], 
                      Year <= input$yearRange[2])
             
             ggplot(data = gdp_per_capita_data, aes(x = Year)) +
               geom_col(data = filter(gdp_per_capita_data, 
                                      `Indicator Name` == 'GDP per capita (current US$)'),
                        aes(y = Value, fill = Value), color = "black") +
               scale_fill_gradientn(colours = c("#03045E", "#012A4A", "#013A63", "#023E8A",
                                                "#014F86", "#277DA1", "#577590", "#4D908E", 
                                                "#43AA8B", "#90BE6D", "#F9C74F", "#F9844A",
                                                "#DC2F02", "#D00000"),
                                    values = seq(0, 1, length.out = 14)) +
               geom_line(data = filter(gdp_per_capita_data, 
                                       `Indicator Name` == 'GDP per capita growth (annual %)'), 
                         aes(y = Value * 100 + 2500), color = "#FFC300", linewidth = 1, group = 1) + 
               scale_y_continuous(sec.axis = sec_axis(~./100 - 25, name = 
                                                        "GDP Per Capita Growth Rate (annual %)")) +
               labs(
                 title = "GDP Per Capita and its Growth Rate Over Time", 
                 x = "Year", y = "GDP Per Capita (current US$)"
               ) +
               theme_minimal() +
               theme(
                 plot.title = element_text(hjust = 0.5),
                 axis.text.x = element_text(angle = 45, hjust = 1),
                 legend.position = "none"
               )
           },
           "Export and Import" = {
             trade_data <- long_data |> 
               filter(`Indicator Name` %in% c('Imports of goods and services (current US$)',
                                              'Exports of goods and services (current US$)'),
                      Year >= input$yearRange[1], 
                      Year <= input$yearRange[2])
             
             ggplot(trade_data, aes(x = Year, y = Value / 10^6, fill = `Indicator Name`)) +
               geom_bar(stat = "identity", position = "stack") +
               scale_fill_manual(values = c('Imports of goods and services (current US$)' = '#003566',
                                            'Exports of goods and services (current US$)' = '#D62828'),
                                 name = "Trade Type", labels = c("Imports", "Exports")) +
               labs(
                 title = "Imports and Exports Over Time", 
                 x = "Year", y = "Value (million current US$)"
               ) +
               theme_minimal() +
               theme(
                 plot.title = element_text(hjust = 0.5),
                 axis.text.x = element_text(angle = 45, hjust = 1)
               )
           }
    )
  })
  
  # 其他标签页的逻辑可以在这里添加
  
               }

shinyApp(ui = ui, server = server)
               