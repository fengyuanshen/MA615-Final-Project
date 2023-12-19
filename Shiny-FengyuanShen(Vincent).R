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
             fluidRow(
               column(6,  # 6 column widths for text
                      h3("Project Overview: An In-Depth Introduction"),
                      p("This project aims to provide users with comprehensive information about the Federated States of Micronesia. It is structured into several key sections to facilitate a thorough understanding of the nation:"),
                      
                      h4("General Description:"),
                      p("This section offers insights into the geographical location, political landscape, economic status, historical development, culture, and surrounding environment of the Federated States of Micronesia. This foundational knowledge sets the stage for a deeper exploration of the nation."),
                      
                      h4("Key Demographics:"),
                      p("Here, the focus shifts to the demographic profile of Micronesia. This includes detailed information on population composition, growth trends, gender ratios, and life expectancy, offering a clear picture of the nation's social structure."),
                      
                      h4("Comparison with Other Regional Island States:"),
                      p("In this segment, the Federated States of Micronesia is compared with neighboring countries such as the Marshall Islands, Nauru, and Palau. This comparison is based on social and economic indicators, providing a regional perspective and highlighting the relative development of these island nations."),
                      
                      h4("SWOT Analysis:"),
                      p("The final section presents a SWOT analysis, examining the strengths, weaknesses, opportunities, and potential threats related to the Federated States of Micronesia. This analysis aims to offer a balanced view of the nation's current status and prospects for future development."),
                      
                      h3("Backgroud Information of Micronesia, Fed. Sts."),
                      p("The Federated States of Micronesia (FSM), also known as Micronesia, is an island country in the Pacific Ocean's Micronesia subregion. It consists of four states - Yap, Chuuk, Pohnpei, and Kosrae - spanning about 607 islands across 2,700 km near the equator. These islands are situated northeast of Indonesia and Papua New Guinea, and south of Guam. Although the FSM's land area is only about 702 km², its marine territory covers nearly 3 million km², ranking it 14th in the world for its exclusive economic zone."),

                      p("Palikir on Pohnpei Island is the capital, while Weno in Chuuk is the largest city. Each state centers around volcanic islands, with many outlying atolls, especially in the Caroline Islands region. The term 'Micronesia' often refers to either the FSM or the broader region comprising several countries."),
                      
                      p("Formerly under U.S. administration as part of the Trust Territory of the Pacific Islands, FSM gained sovereignty on November 3, 1986, through a Compact of Free Association with the U.S., after establishing its constitution on May 10, 1979. It's a United Nations member and part of the Pacific Community since 1983.")
               ),
               column(6,  # 6 column widths for pictures
                      img(src = "pic/introduction.jpg", style = "width: 100%; height: auto; object-fit: contain;")
               )
             )
    ),
    tabPanel("General Description",
             tabsetPanel(
               tabPanel("Map of the Island State", 
                        h3("Map Description:"),
                        p("This map provides a detailed view of the Federated States of Micronesia, highlighting its four states - Pohnpei, Chuuk, Kosrae, and Yap. Users can select a state from the dropdown menu to zoom in and explore more specific geographical details."),
                        
                        selectInput("stateSelect", "Select State", choices = c("Pohnpei", "Chuuk", "Kosrae", "Yap")),
                        leafletOutput("selectedStateMap")
               ),
               tabPanel("World Map Highlighting the Island State's Location",
                        h3("Map Description:"),
                        p("This detailed map provides a clear visualization of the geographical positions of the four states - Pohnpei, Chuuk, Kosrae, and Yap - on the global landscape. Each state is distinctly color-coded for easy identification: Pohnpei in vibrant red, Chuuk in a deep blue, Kosrae in a lush green, and Yap in a bright yellow. The map is designed with an interactive feature, allowing users to zoom in for a closer look and uncover more intricate geographical details."),
                        leafletOutput("worldMap")
               ),
               tabPanel("Economic Characteristics of the Island State",
                        h3("Chart Description:"),
                        p("This section primarily explores the economic status of the Federated States of Micronesia. Users can engage with an array of charts depicting various economic indicators, such as GDP and its growth rate, per capita GDP, and import-export values, through a drop-down menu selection. Additionally, there is an interactive slider feature that enables users to select a specific year range, providing a more detailed view of the economic development over time."),
                        selectInput("selectedChart", "Choose a Chart", 
                                    choices = c("GDP and GDP Growth", "GDP Per Capita", "Export and Import")),
                        sliderInput("yearRangeEconomic", "Select Year Range", 
                                    min = 1986, max = 2022, value = c(1986, 2022)),
                        plotOutput("selectedChartPlot")
               ),
               tabPanel("Overview of Political, Social, Cultural, and Other Aspects",
                        p("This section will offer a brief overview of additional relevant information pertaining to the Federated States of Micronesia:"),
                        
                        fluidRow(
                          column(6,  # 6 column widths for text
                                 h4("Politics"),
                                 p("Gained independence from US-administered Trust Territory of the Pacific Islands in 1986. It's a democratic nation with a Compact of Free Association with the United States."),
                                 
                                 h4("Economy"),
                                 p("Relies on subsistence farming, fishing, and US financial aid. Limited natural resources; potential for tourism development is hindered by remoteness."),
                                 
                                 h4("History"),
                                 p("Inhabited for over 4,000 years; influenced by Spanish, German, and Japanese rule. Significant World War II site."),
                                 
                                 h4("Population and Culture"),
                                 p("Predominantly Micronesian population, with diverse ethnolinguistic groups. Rich in traditions and customs, with notable sites like Yap's Rai stones and Pohnpei's Nan Madol ruins."),
                                 
                                 h4("Environment"),
                                 p("Tropical rainforest climate, characterized by high rainfall, lush landscapes, and unique ecosystems.")
                          ),
                          column(6,  # 6 column widths for pictures
                                 img(src = "pic/overview1.jpg", style = "width: 100%; height: auto; object-fit: contain;"),
                                 img(src = "pic/overview2.jpg", style = "width: 100%; height: auto; object-fit: contain;")
                          )
                        )
               )
             )
    ),
    tabPanel("Key Demographics",
             p("In this section, users can explore the demographic data of the Federated States of Micronesia by selecting different types of charts via clickable buttons. This includes information on annual total population, population growth rate, demographic composition, gender ratio, and life expectancy. Additionally, an interactive slider allows for the selection of specific year ranges, providing a more in-depth understanding of the country's demographic trends over time."),
             p("Overall, the Federated States of Micronesia has experienced a gradual population growth, characterized by a remarkably stable gender ratio. Post-World War II, there has been a steady increase in life expectancy, although this improvement has plateaued in recent years, indicating a more moderate pace of growth in this aspect."),
             
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
             p("In this section, my project conducts a comparative analysis between the Federated States of Micronesia and its neighboring countries (Marshall Islands, Nauru, and Palau), focusing on various social and economic indicators to provide a comprehensive view of the region's overall development. Users can select different indicators for comparison through a drop-down menu."),
             p("Notably, within this region, the Federated States of Micronesia boasts the highest GDP and the largest population. However, its GDP per capita does not stand out as particularly high. Its export figures are moderate, while its import expenditures are considerably higher. An intriguing point of contrast is that, although Micronesia has the lowest Human Development Index among these four nations, it surprisingly leads in terms of life expectancy."),
             
             selectInput("indicatorType", "Select Indicator Type for Comparison", 
                         choices = c("Social Indicator", "Economic Indicator")),
             uiOutput("indicatorDisplay")
    ),
    tabPanel("SWOT Analysis",
             tabsetPanel(
               tabPanel("Strengths",
                        column(5,  # 5 column widths for text
                               h3("Strengths"),
                               HTML("
                                  <ol>
                                    <li>Strategic Location: Situated in the Western Pacific Ocean, Micronesia's extensive exclusive economic zone offers significant marine resources.</li>
                                    <br>
                                    <li>U.S. Support: The Compact of Free Association with the United States provides substantial financial aid and development support.</li>
                                    <br>
                                    <li>Natural Resources: The primary economic sector is commercial fishing, with significant income from fishing rights sales and offshore corporate registrations for captive insurance.</li>
                                    <br>
                                    <li>Low Debt Levels: FSM has a relatively low government debt-to-GDP ratio, indicating financial stability.</li>
                                  </ol>
                                ")
                        ),
                        column(7, # 7 column widths for pictures
                               # Insert local picture
                               img(src = "pic/strengths.jpg", style = "width: 100%; height: auto; object-fit: contain;")
                        )
               ),
               tabPanel("Weaknesses",
                        column(5,  # 5 column widths for text
                               h3("Weaknesses"),
                               HTML("
                                  <ol>
                                    <li>Economic Dependence: Heavy reliance on U.S. funding and grants, with a need to diversify economic sources.</li>
                                    <br>
                                    <li>Limited Foreign Investment: Restrictions on foreign ownership and challenges in registering businesses deter foreign direct investment.</li>
                                    <br>
                                    <li>Weak Infrastructure: Poor infrastructure and high costs of imported goods and services hamper economic growth.</li>
                                    <br>
                                    <li>Subsistence Economy: Apart from a few urban centers, most of the economy is subsistence-based, limiting commercial activity.</li>
                                  </ol>
                                ")
                        ),
                        column(7,  # 7 column widths for pictures
                               # Insert local picture
                               img(src = "pic/weaknesses.jpg", style = "width: 100%; height: auto; object-fit: contain;")
                        )
               ),
               tabPanel("Opportunities",
                        column(5,  # 5 column widths for text
                               h3("Opportunities"),
                               HTML("
                                  <ol>
                                    <li>Economic Diversification: Opportunities to diversify the economy beyond fishing and government employment.</li>
                                    <br>
                                    <li>Tourism Potential: Natural beauty and unique culture offer potential for eco-tourism development.</li>
                                    <br>
                                    <li>Trust Fund Utilization: Transitioning to using proceeds from a trust fund developed from U.S. contributions for development.</li>
                                  </ol>
                                ")
                        ),
                        column(7,  # 7 column widths for pictures
                               # Insert local picture
                               img(src = "pic/opportunities.jpg", style = "width: 100%; height: auto; object-fit: contain;")
                        )
               ),
               tabPanel("Threats",
                        column(5,  # 5 column widths for text
                               h3("Threats"),
                               HTML("
                                  <ol>
                                    <li>Reduction in Compact Funding: The shift in Compact funding methodology in 2023 poses financial challenges.</li>
                                    <br>
                                    <li>Political Instability: Lack of political parties and potential for instability could affect economic decisions.</li>
                                    <br>
                                    <li>Environmental Risks: As an island nation, Micronesia faces significant risks from climate change and natural disasters.</li>
                                  </ol>
                                ")
                        ),
                        column(7,  # 7 column widths for pictures
                               # Insert local picture
                               img(src = "pic/threats.jpg", style = "width: 100%; height: auto; object-fit: contain;")
                        )
               )
             )
    ),
    tabPanel("Reference",
             div(
               h3("References and Further Reading:"),
               p(a("Wikipedia - Micronesia", href = "https://en.wikipedia.org/wiki/Micronesia", target = "_blank")),
               p(a("Wikipedia - Federated States of Micronesia", href = "https://en.wikipedia.org/wiki/Federated_States_of_Micronesia#Culture", target = "_blank")),
               p(a("Wikipedia - Palikir", href = "https://en.wikipedia.org/wiki/Palikir", target = "_blank")),
               p(a("Wikipedia - Administrative divisions of the Federated States of Micronesia", href = "https://en.wikipedia.org/wiki/Administrative_divisions_of_the_Federated_States_of_Micronesia", target = "_blank")),
               p(a("Wikipedia - Marshall Islands", href = "https://en.wikipedia.org/wiki/Marshall_Islands", target = "_blank")),
               p(a("Wikipedia - Nauru", href = "https://en.wikipedia.org/wiki/Nauru", target = "_blank")),
               p(a("Wikipedia - Palau", href = "https://en.wikipedia.org/wiki/Palau", target = "_blank")),
               p(a("Simplemaps - Micronesia", href = "https://simplemaps.com/data/fm-cities", target = "_blank")),
               p(a("UNData", href = "https://data.un.org", target = "_blank")),
               p(a("World Bank Open Data", href = "https://data.worldbank.org/", target = "_blank")),
               p(a("IMF DATA", href = "https://www.imf.org/en/Data", target = "_blank")),
               p(a("Federated States of Micronesia - Spotlight shines brightly on “The Big Ocean State”", href = "https://statemag.state.gov/2021/07/0721pom/", target = "_blank")),
               p(a("US Department of State - 2021 Investment Climate Statements: Micronesia", href = "https://www.state.gov/reports/2021-investment-climate-statements/micronesia/", target = "_blank")),
               p(a("R for Data Science (2e)", href = "https://r4ds.hadley.nz/", target = "_blank")),
               p(a("Mastering Shiny", href = "https://mastering-shiny.org/index.html", target = "_blank")),
               p(a("How to Publish Shiny", href = "https://shiny.posit.co/r/getstarted/shiny-basics/lesson7/", target = "_blank")),
               p("Lastly, I would like to extend my profound gratitude to GPT-4 for its invaluable assistance in debugging my work.")
             )
    )
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
  

  # The dynamic UI displays different content depending on the selection
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
  
  flags <- c("pic/Flag_of_the_Federated_States_of_Micronesia.svg", 
             "pic/Flag_of_the_Marshall_Islands.svg", "pic/Flag_of_Nauru.svg", "pic/Flag_of_Palau.svg")
  
  output$indicatorDisplay <- renderUI({
    if(input$indicatorType == "Social Indicator") {
      # Displays tables and pictures
      list(
        gt::gt_output("socialTable"),  # Render gt table
        # Create a div that contains all the flag images and resize them
        tags$div(
          style = "text-align: center; margin-bottom: 40px;",  # Add a style to center the image and add bottom spacing
          lapply(flags, function(flag) {
            tags$img(src = flag, style = "height: 150px; margin: 10px; display: inline-block;")  # Adjust the height and display mode
          }),
          tags$p("The image above displays the national flags of four Pacific island nations. From left to right, they represent the Federated States of Micronesia, the Marshall Islands, Nauru, and Palau, each with its unique design and colors.",
                 style = "text-align: center; font-size: 18px;")  # Center the text
        )
      )
    } else if(input$indicatorType == "Economic Indicator") {
      # Show graphs
      plot_output_list <- lapply(1:4, function(i) {
        plotOutput(outputId = paste0("plot", i))
      })
      do.call(tags$div, c(plot_output_list, style = "display: grid; grid-template-columns: repeat(2, 1fr); grid-gap: 20px;"))
    }
  })
  
  # Generate a social indicator table
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
  
               }

shinyApp(ui = ui, server = server)
               