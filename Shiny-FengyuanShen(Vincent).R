library(tidyverse)
library(ggplot2)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(treemap)
library(treemapify)
library(gt)
library(gtExtras)

# Read the CSV file and skip the first 4 lines
micronesia_data <- read_csv("data/API_FSM_DS2_en_csv_v2_6235080.csv", skip = 4)

ui <- shinyUI(fluidPage(
  titlePanel("Micronesia Island State Analysis"),
  # Add my name and Github link
  div(style = "margin-bottom: 20px;",    # Add some space below the author line
      h4("Author: Fengyuan (Vincent) Shen. All the code can be accessed through my ",
         a("Github Repository", href = "https://github.com/fengyuanshen/MA615-Final-Project", target = "_blank")
      )
  ),
  
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
                        sliderInput("yearRangeEconomic", "Select Year Range", 
                                    min = 1986, max = 2022, value = c(1986, 2022)),
                        plotOutput("selectedChartPlot")
               ),
               tabPanel("Overview of Political, Social, Cultural, and Environmental Aspects")
             )
    ),
    tabPanel("Key Demographics",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("selectedChartType", "Select Chart Type",
                              choices = c("Population and its Growth Rate", 
                                          "Demographic Composition", 
                                          "Male vs Female", 
                                          "Life Expectancy")),
                 uiOutput("slider")
               ),
               mainPanel(
                 plotOutput("demographicsPlot")
               )
             )
    ),
    tabPanel("Comparison with Other Regional Island States",
             selectInput("indicatorType", "Select Indicator Type for Comparison", 
                         choices = c("Social Indicator", "Economy Indicator")),
             uiOutput("indicatorDisplay")
    ),
    tabPanel("SWOT Analysis",
             tabsetPanel(
               tabPanel("Strengths",
                        column(8,  # 8列宽度用于文本
                               h3("Strengths"),
                               HTML("
                                  <ol>
                                    <li>Strategic Location: Situated in the Western Pacific Ocean, Micronesia's extensive exclusive economic zone offers significant marine resources.</li>
                                    <br>
                                    <li></li>
                                    <br>
                                    <li></li>
                                    <br>
                                    <li></li>
                                  </ol>
                                ")
                        ),
                        column(4,  # 4列宽度用于图片
                               uiOutput("strengthsImage")
                               # 这里预留给图片
                        )
               ),
               tabPanel("Weaknesses",
                        column(8,  # 8列宽度用于文本
                               h3("Weaknesses"),
                               HTML("
                                  <ol>
                                    <li>Strategic Location: Situated in the Western Pacific Ocean, Micronesia's extensive exclusive economic zone offers significant marine resources.</li>
                                    <br>
                                    <li></li>
                                    <br>
                                    <li></li>
                                    <br>
                                    <li></li>
                                  </ol>
                                ")
                        ),
                        column(4,  # 4列宽度用于图片
                               uiOutput("strengthsImage")
                               # 这里预留给图片
                        )
               ),
               tabPanel("Opportunities",
                        column(8,  # 8列宽度用于文本
                               h3("Opportunities"),
                               HTML("
                                  <ol>
                                    <li>Strategic Location: Situated in the Western Pacific Ocean, Micronesia's extensive exclusive economic zone offers significant marine resources.</li>
                                    <br>
                                    <li></li>
                                    <br>
                                    <li></li>
                                    <br>
                                    <li></li>
                                  </ol>
                                ")
                        ),
                        column(4,  # 4列宽度用于图片
                               uiOutput("strengthsImage")
                               # 这里预留给图片
                        )
               ),
               tabPanel("Threats",
                        column(8,  # 8列宽度用于文本
                               h3("Threats"),
                               HTML("
                                  <ol>
                                    <li>Strategic Location: Situated in the Western Pacific Ocean, Micronesia's extensive exclusive economic zone offers significant marine resources.</li>
                                    <br>
                                    <li></li>
                                    <br>
                                    <li></li>
                                    <br>
                                    <li></li>
                                  </ol>
                                ")
                        ),
                        column(4,  # 4列宽度用于图片
                               uiOutput("strengthsImage")
                               # 这里预留给图片
                        )
               )
             )
    ),
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
                             'Travel services (% of commercial service exports)',
                             'Population, female (% of total population)',
                             'Population, male (% of total population)',
                             'Life expectancy at birth, total (years)')
    
    # Filter data
    filtered_data <- micronesia_data |> 
      filter(`Indicator Name` %in% selected_indicators) |> 
      select(-c(`Country Name`, `Country Code`, `Indicator Code`))
    
    # Convert data to long format
    long_data <- filtered_data |> 
      pivot_longer(cols = `1986`:`2022`, names_to = "Year", values_to = "Value") |> 
      mutate(Year = as.numeric(Year))  # Convert year to numeric
    
    # Select data and plotting logic based on the chart type chosen by the user
    req(input$selectedChart)  # Make sure the chart is selected
    selected_data <- long_data |> 
      filter(Year >= input$yearRangeEconomic[1], Year <= input$yearRangeEconomic[2])
    
    # Use switch to draw different charts based on the selection
    switch(input$selectedChart,
           "GDP and GDP Growth" = {
             gdp_growth_data <- long_data |> 
               filter(`Indicator Name` %in% c('GDP (current US$)', 
                                              'GDP growth (annual %)'),
                      Year >= input$yearRangeEconomic[1], Year <= input$yearRangeEconomic[2])
             
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
                      Year >= input$yearRangeEconomic[1], Year <= input$yearRangeEconomic[2])
             
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
                      Year >= input$yearRangeEconomic[1], Year <= input$yearRangeEconomic[2])
             
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
  
  # Sliding axis
  output$slider <- renderUI({
    if (input$selectedChartType %in% c("Population and its Growth Rate", 
                                       "Male vs Female", 
                                       "Life Expectancy")) {
      sliderInput("yearRangeDemographics", "Select Year Range", 
                  min = 1986, max = 2022, value = c(1986, 2022))
    }
  })
  
  # Render the chart according to the selection
  output$demographicsPlot <- renderPlot({
    
    # A list of selected indicators
    selected_indicators <- c('GDP (current US$)', 'GDP growth (annual %)', 
                             'GDP per capita (current US$)', 
                             'GDP per capita growth (annual %)',
                             'Imports of goods and services (current US$)', 
                             'Exports of goods and services (current US$)', 
                             'Population, total', 'Population growth (annual %)', 
                             'Travel services (% of commercial service exports)',
                             'Population, female (% of total population)',
                             'Population, male (% of total population)',
                             'Life expectancy at birth, total (years)')
    
    # Filter data
    filtered_data <- micronesia_data |> 
      filter(`Indicator Name` %in% selected_indicators) |> 
      select(-c(`Country Name`, `Country Code`, `Indicator Code`))
    
    # Convert data to long format
    long_data <- filtered_data |> 
      pivot_longer(cols = `1986`:`2022`, names_to = "Year", values_to = "Value") |> 
      mutate(Year = as.numeric(Year))  # Convert year to numeric
    
    req(input$selectedChartType)
    switch(input$selectedChartType,
           "Population and its Growth Rate" = {
             population_data <- long_data |> 
               filter(`Indicator Name` %in% c('Population, total', 
                                              'Population growth (annual %)'),
                      Year >= input$yearRangeDemographics[1], Year <= input$yearRangeDemographics[2])
             
             ggplot(data = population_data, aes(x = Year)) +
               geom_col(data = filter(population_data, `Indicator Name` == 'Population, total'), 
                        aes(y = Value / 1000, fill = Value), color = "black") +
               scale_fill_gradientn(colours = c("#D00000", "#DC2F02", "#F3722C", "#F9844A",
                                                "#F9C74F", "#FFE347", "#90BE6D", "#4D908E",
                                                "#43AA8B", "#006F57", "#277DA1", "#014F86"),
                                    values = seq(0, 1, length.out = 15)) +
               geom_line(data = filter(population_data, 
                                       `Indicator Name` == 'Population growth (annual %)'), 
                         aes(y = Value * 10 + 90), color = "#FFC300", linewidth = 1, group = 1) +
               scale_y_continuous(sec.axis = sec_axis(~./10 - 9, 
                                                      name = "Population Growth Rate (annual %)")) +
               labs(
                 title = "Population and Population Growth Rate Over Time", 
                 x = "Year", y = "Total Population (thousand)"
               ) +
               theme_minimal() +
               theme(
                 plot.title = element_text(hjust = 0.5),
                 axis.text.x = element_text(angle = 45, hjust = 1),
                 legend.position = "none"
               )
           },
             "Demographic Composition" = {
               # Create a demographic dataframe
               composition_data <- data.frame(
                 Group = c("Chuukese", "Pohnpeian", "Kosraean", "Yapese", 
                           "Yap Outer Islands", "Asian", "Polynesian", "Other", "Unknown"),
                 Percentage = c(48.8, 24.2, 6.2, 5.2, 4.5, 1.8, 1.5, 6.4, 1.4)
               )
               
               # Add a label with a percentage
               composition_data$Label <- paste(composition_data$Group, "\n", 
                                               round(composition_data$Percentage,2), "%", sep="")
               
               # Draw a treemap and add labels
               ggplot(composition_data, aes(area = Percentage, fill = Group, label = Label)) +
                 geom_treemap(colour = "black", size = 1.5) +
                 geom_treemap_text(colour = "white", place = "centre", grow = FALSE, reflow = TRUE) +
                 labs(title = "Population Composition by Ethnolinguistic Group") +
                 theme_minimal() +
                 theme(
                   plot.title = element_text(size = 16, hjust = 0.5),
                   legend.position = "none"
                 ) +
                 scale_fill_manual(values = c("#D00000", "#BB010B", "#F9844A", "#F9C74F", "#90BE6D",
                                              "#43AA8B", "#277DA1", "#014F86", "#023E8A", "#03045E"))
             },
               "Male vs Female" = {
                 proportion_data <- long_data |> 
                   filter(`Indicator Name` %in% c('Population, female (% of total population)', 'Population, male (% of total population)'),
                          Year >= input$yearRangeDemographics[1], Year <= input$yearRangeDemographics[2])
                 
                 ggplot(proportion_data, aes(x = Year, y = Value , fill = `Indicator Name`)) +
                   geom_bar(stat = "identity", position = "stack") +
                   scale_fill_manual(values = c('Population, male (% of total population)' = '#003566',
                                                'Population, female (% of total population)' = '#D62828'),
                                     name = "Gender", labels = c("Male Proportion", "Female Proportion")) +
                   labs(
                     title = "Male and Female Population Proportion",
                     x = "Year", y = "Population Proportion (%)") +
                   theme_minimal() +
                   theme(
                     plot.title = element_text(hjust = 0.5),
                     axis.text.x = element_text(angle = 45, hjust = 1)
                   )
               },
                 "Life Expectancy" = {
                   life_expectancy_data <- long_data |> 
                     filter(`Indicator Name` %in% 'Life expectancy at birth, total (years)',
                            Year >= input$yearRangeDemographics[1], Year <= input$yearRangeDemographics[2])
                   
                   ggplot(life_expectancy_data, aes(x = Year, y = Value)) +
                     geom_line(color = "#0077B6", linewidth = 1) +
                     geom_point(color = "#CD1624", size = 2) +
                     labs(
                       title = "Life Expectancy at Birth", 
                       x = "Year", y = "Life Expectancy (Years)"
                       ) +
                     theme_minimal() +
                     theme(
                       plot.title = element_text(hjust = 0.5),
                       axis.text.x = element_text(angle = 45, hjust = 1)
                     )
                 }
    )
    })
  

  # 动态UI根据选择展示不同的内容
  # Create dataframe
  country_data <- data.frame(
    Country = c("Micronesia, Fed. Sts.", "Marshall Islands", "Nauru", "Palau"),
    Population = c(114164, 41569, 12668, 18055),
    PopulationGrowth = c(0.9, -1.2, 1.2, 0.2),
    PopDensity = c(166.1, 331.2, 543.6, 39.5),
    NetMigration = c(-635.0, -1901, -66, 3),
    LifeExpectancy = c(71, 65, 64, 69),
    SexRatio = c(103.4, 104, 101.8, 106),
    SurfaceArea = c(702, 181, 21, 459),
    HumanDevelopmentIndex = c(0.628, 0.639, 0.72, 0.767),
    CapitalCity = c("Palikir", "Majuro", "Yaren", "Ngerulmud"),
    Currency = c("USD", "USD", "AUD", "USD")
  )
  
  # Use gt to generate a table
  gt_table <- gt(data = country_data) |> 
    tab_header(
      title = "Regional Island States: A Comparison of Social Indicators"
    ) |> 
    cols_label(
      Country = "Country",
      Population = "Population (2022)",
      PopulationGrowth = "Population growth (annual %, 2022)",
      PopDensity = "Pop. density (per km^2, 2021)",
      NetMigration = "Net migration (2021)",
      LifeExpectancy = "Life Expectancy at Birth, Total (years, 2021)",
      SexRatio = "Sex ratio (male per 100 female, 2022)",
      SurfaceArea = "Surface Area (km^2)",
      HumanDevelopmentIndex = "Human Development Index (2021)",
      CapitalCity = "Capital City",
      Currency = "Currency"
    ) |> 
    # Adjust column
    cols_width(
      Country ~ px(170),
      Population ~ px(100),
      PopulationGrowth ~ px(170),
      PopDensity ~ px(170),
      NetMigration ~ px(110),
      LifeExpectancy ~ px(200),
      SexRatio ~ px(160),
      SurfaceArea ~ px(130),
      HumanDevelopmentIndex ~ px(170),
      CapitalCity ~ px(100),
      Currency ~ px(100)
    ) |> 
    tab_options(
      row.striping.include_table_body = TRUE
    ) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = everything())
    ) |> 
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_column_labels(columns = everything())
    )
  
  flags <- c("flags/Flag_of_the_Federated_States_of_Micronesia.svg", 
             "flags/Flag_of_the_Marshall_Islands.svg", "flags/Flag_of_Nauru.svg", "flags/Flag_of_Palau.svg")
  
  output$indicatorDisplay <- renderUI({
    if(input$indicatorType == "Social Indicator") {
      # 显示表格和图片
      list(
        gt::gt_output("socialTable"),  # 渲染gt表格
        # 创建包含所有国旗图片的div，调整图片大小
        tags$div(
          style = "text-align: center; margin-bottom: 40px;",  # 添加样式使图片居中并添加底部间距
          lapply(flags, function(flag) {
            tags$img(src = flag, style = "height: 150px; margin: 10px; display: inline-block;")  # 调整高度和显示方式
          }),
          tags$p("The image above displays the national flags of four Pacific island nations. From left to right, they represent the Federated States of Micronesia, the Marshall Islands, Nauru, and Palau, each with its unique design and colors.",
                 style = "text-align: center; font-size: 18px;")  # 文本居中
        )
      )
    } else if(input$indicatorType == "Economy Indicator") {
      # 显示图表
      plot_output_list <- lapply(1:4, function(i) {
        plotOutput(outputId = paste0("plot", i))
      })
      do.call(tags$div, c(plot_output_list, style = "display: grid; grid-template-columns: repeat(2, 1fr); grid-gap: 20px;"))
    }
  })
  
  # 生成社会指标表
  output$socialTable <- gt::render_gt({
    gt_table
  })
  
  # Render a chart of economy indicators
  # Create dataframe
  countries <- c("Micronesia", "Marshall Islands", "Nauru", "Palau")
  economy <- data.frame(
    Country = rep(countries, each=3),
    Year = rep(c("2010", "2015", "2021"), times=4),
    GDP = c(297, 316, 414, 168, 181, 237, 59, 104, 133, 183, 279, 280),
    GDP_Per_Capita = c(2885, 2906.5, 3640.4, 2986.1, 3159.2, 4038.2, 5924.8, 10050.4, 
                       12350.9, 10219.9, 15784, 15572.5),
    Exports = c(23, 40, 34, 17, 25, 23, 25, 61, 54, 12, 6, 7),
    Imports = c(168, 160, 189, 76, 84, 72, 23, 109, 140, 107, 150, 216)
  )
  
  # GDP Comparison
  plot1 <- ggplot(economy, aes(x=Country, y=GDP, fill=Year)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values = c("#D62828", "#F77F00", "#457B9D")) + 
    geom_text(aes(label=GDP), vjust=-0.3, position=position_dodge(width=0.9), size=3.5) + 
    labs(
      title = "GDP Comparison", 
      x = "", y = "GDP (million current US$)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  # GDP Per Capita Comparison
  plot2 <- ggplot(economy, aes(x=Country, y=GDP_Per_Capita, fill=Year)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values = c("#D62828", "#F77F00", "#457B9D")) + 
    geom_text(aes(label=GDP_Per_Capita), position=position_dodge(width=0.9), 
              vjust=0.5, hjust=1.2, size=3.5, angle=90, colour="white") +
    labs(
      title = "GDP Per Capita Comparison", 
      x = "", y = "GDP Per Capita (current US$)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  # Imports Comparison
  plot3 <- ggplot(economy, aes(x=Country, y=Imports, fill=Year)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values = c("#D62828", "#F77F00", "#457B9D")) + 
    geom_text(aes(label=Imports), vjust=-0.3, position=position_dodge(width=0.9), size=3.5) + 
    labs(
      title = "Imports Comparison", 
      x = "", y = "Imports (million current US$)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  # Exports Comparison
  plot4 <- ggplot(economy, aes(x=Country, y=Exports, fill=Year)) +
    geom_bar(stat="identity", position=position_dodge()) +
    scale_fill_manual(values = c("#D62828", "#F77F00", "#457B9D")) + 
    geom_text(aes(label=Exports), vjust=-0.3, position=position_dodge(width=0.9), size=3.5) + 
    labs(
      title = "Exports Comparison", 
      x = "", y = "Exports (million current US$)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
  
  plots <- list(plot1, plot2, plot3, plot4)
  lapply(1:4, function(i) {
    output[[paste0("plot", i)]] <- renderPlot({
      plots[[i]]
    })
  })
  
  # 其他标签页的逻辑可以在这里添加
  
               }

shinyApp(ui = ui, server = server)
               