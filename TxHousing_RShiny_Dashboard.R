library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(corrplot)
library(leaflet)
library(viridis)
library(scales)
library(scales) 
library(rsconnect)
# Load the dataset
data("txhousing", package = "ggplot2")
# City latitude and longitude
city_coords <- data.frame(
  city = c("Abilene", "Amarillo", "Arlington", "Austin", "Bay Area", "Beaumont", 
           "Brazoria County", "Brownsville", "Bryan-College Station", "Collin County",
           "Corpus Christi", "Dallas", "Denton County", "El Paso", "Fort Bend",
           "Fort Worth", "Galveston", "Garland", "Harlingen", "Houston", "Irving",
           "Kerrville", "Killeen-Fort Hood", "Laredo", "Longview-Marshall", "Lubbock", 
           "Lufkin", "McAllen", "Midland", "Montgomery County", "Nacogdoches",
           "NE Tarrant County", "Odessa", "Paris", "Port Arthur", "San Angelo",
           "San Antonio", "San Marcos", "Sherman-Denison", "South Padre Island",
           "Temple-Belton", "Texarkana", "Tyler", "Victoria", "Waco", "Wichita Falls"),
  lat = c(32.4487, 35.2219, 32.7357, 30.2672, 29.6847, 30.0802,
          29.1725, 25.9017, 30.6279, 33.1779, 27.8006, 32.7767,
          33.2148, 31.7619, 29.5693, 32.7555, 29.3013, 32.9126, 
          26.1906, 29.7604, 32.8140, 30.0474, 31.1171, 27.5064, 
          32.5007, 33.5779, 31.3382, 26.2034, 31.9974, 30.3889, 
          31.6035, 32.8783, 31.8457, 33.6609, 29.8849, 31.4638, 
          29.4241, 29.8833, 33.6357, 26.1014, 31.0982, 33.4418,
          32.3513, 28.8053, 31.5493, 33.9137),
  lon = c(-99.7331, -101.8313, -97.1081, -97.7431, -95.0367, -94.1266,
          -95.4314, -97.4975, -96.3344, -96.6825, -97.3964, -96.7970,
          -97.1331, -106.4850, -95.6458, -97.3308, -94.7977, -96.6389, 
          -97.6961, -95.3698, -96.9489, -99.1410, -97.7278, -99.5075, 
          -94.7360, -101.8552, -94.7291, -98.2300, -102.0779, -95.6903, 
          -94.6555, -97.3285, -102.3677, -95.5562, -100.4939, -100.4370,
          -98.4936, -97.9397, -96.6039, -97.1685, -97.3428, -94.0424,
          -95.3011, -96.9785, -97.1467, -98.4934)
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Texas Housing Market Analysis"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Market Performance", tabName = "market_performance", icon = icon("line-chart")),
      menuItem("Seasonal Patterns", tabName = "seasonal", icon = icon("calendar")),
      menuItem("City Comparison", tabName = "city_comparison", icon = icon("building")),
      menuItem("Market Dynamics", tabName = "market_dynamics", icon = icon("chart-bar"))
    ),
    conditionalPanel(
      condition = "input.tabs != 'overview'",
    selectizeInput("city", "Select Cities:", 
                   choices = unique(txhousing$city), 
                   selected = "Austin", 
                   multiple = TRUE,
                   options = list(placeholder = 'Select cities')),
    sliderInput("year", "Select Year Range:",
                min = min(txhousing$year),
                max = max(txhousing$year),
                value = c(min(txhousing$year), max(txhousing$year)),
                step = 1,
                sep = "")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Texas Housing Market Dashboard", 
                  width = 12, 
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  p("This dashboard provides an interactive analysis of the Texas housing market 
               to support policymakers in making informed real estate policies."),
                  p("It includes key insights into home prices, sales trends, seasonal 
               patterns, and inventory dynamics across different cities."),
                  p("The data comes from the Texas Real Estate Research Center and covers the period from 1990 to 2015.")
                )
              ),
              fluidRow(
                valueBoxOutput("median_price_summary", width = 4),
                valueBoxOutput("total_sales_summary", width = 4),
                valueBoxOutput("inventory_summary", width = 4)
              ),
              fluidRow(
                box(plotlyOutput("average_price_sales_plot"), width = 12)
              )
    ),
    tabItem(tabName = "market_performance",
            fluidRow(
              box(plotlyOutput("price_trends_plot"), width = 12)
            ),
            fluidRow(
              box(plotlyOutput("sales_trends_plot"), width = 12)
            )
    ),
      # Seasonal Patterns Tab
      tabItem(tabName = "seasonal",
              fluidRow(
                box(plotlyOutput("seasonal_heatmap"), width = 12)
              ),
              fluidRow(
                box(plotlyOutput("monthly_sales_plot"), width = 12)
              )
      ),
      
      # City Comparison Tab
    tabItem(tabName = "city_comparison",
            fluidRow(
              box(plotlyOutput("city_comparison_plot"), width = 6),
              box(leafletOutput("city_comparison_map"), width = 6)  
            ),
            fluidRow(
              box(plotlyOutput("inventory_price_plot"), width = 12)
            )),
      
      # Market Dynamics Tab
    tabItem(tabName = "market_dynamics",
            fluidRow(
              box(plotlyOutput("sales_volume_plot"), width = 6),
              box(plotlyOutput("total_volume_plot"), width = 6)
            ),
            fluidRow(
              box(plotlyOutput("inventory_trends_plot"), width = 6),
              box(plotlyOutput("price_volume_bubble"), width = 6)
            )
    )
    )
  )
)

# Server
server <- function(input, output, session) {
  observe({
    updateTabsetPanel(session, "tabs", selected = input$tabs)
  })
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    txhousing %>% 
      filter(city %in% input$city,
             year >= input$year[1],
             year <= input$year[2])
  })
  output$median_price_summary <- renderValueBox({
    median_price <- median(txhousing$median, na.rm = TRUE)
    valueBox(
      paste0("$", format(median_price, big.mark = ",")), 
      subtitle = "Median Home Price (Texas)", 
      icon = icon("home"), 
      color = "blue"
    )
  })
  
  output$total_sales_summary <- renderValueBox({
    total_sales <- sum(txhousing$sales, na.rm = TRUE)
    valueBox(
      format(total_sales, big.mark = ","), 
      subtitle = "Total Home Sales (Texas)", 
      icon = icon("chart-line"), 
      color = "green"
    )
  })
  
  output$inventory_summary <- renderValueBox({
    avg_inventory <- mean(txhousing$inventory, na.rm = TRUE)
    valueBox(
      paste0(round(avg_inventory, 1), " months"), 
      subtitle = "Average Inventory (Texas)", 
      icon = icon("warehouse"), 
      color = "orange"
    )
  })
  # Overview Tab
  output$average_price_sales_plot <- renderPlotly({
    price_sales_trends <- txhousing %>%
      group_by(year) %>%
      summarise(
        avg_price = mean(median, na.rm = TRUE),
        total_sales = sum(sales, na.rm = TRUE)
      )
    
    plot_ly(price_sales_trends) %>%
      add_trace(x = ~year, y = ~avg_price, name = "Avg Price", type = "scatter", mode = "lines+markers", yaxis = "y1", line = list(color = "blue")) %>%
      add_trace(x = ~year, y = ~total_sales, name = "Total Sales", type = "scatter", mode = "lines+markers", yaxis = "y2", line = list(color = "green")) %>%
      layout(
        title = "Average Home Price & Total Sales Over Time",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Average Home Price (USD)", side = "left"),
        yaxis2 = list(title = "Total Sales", side = "right", overlaying = "y"),
        legend = list(title = list(text = "Metrics"))
      )
  })
  # Price Trends Tab
  output$price_trends_plot <- renderPlotly({
    price_trends <- filtered_data() %>%
      group_by(date, city) %>%
      summarise(median_price = median(median, na.rm = TRUE), .groups = 'drop')
    
    plot_ly(price_trends, x = ~date, y = ~median_price, color = ~city, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Median Home Prices Over Time in Selected Cities",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Median Price (USD)"),
             legend = list(title = list(text = "City")))
  })
  
  output$median_prices_plot <- renderPlotly({
    median_prices <- filtered_data() %>%
      group_by(city) %>%
      summarise(median_price = median(median, na.rm = TRUE)) %>%
      arrange(desc(median_price))
    
    plot_ly(median_prices, x = ~reorder(city, median_price), y = ~median_price, type = "bar", marker = list(color = ~median_price, colorscale = "Viridis")) %>%
      layout(title = "Median Home Prices Across Selected Texas Cities",
             xaxis = list(title = "City"),
             yaxis = list(title = "Median Price (USD)"))
  })
  
  # Seasonal Patterns Tab
  output$seasonal_heatmap <- renderPlotly({
    seasonal_sales <- filtered_data() %>%
      group_by(year, month) %>%
      summarise(total_sales = sum(sales, na.rm = TRUE)) %>%
      filter(!is.na(total_sales))  # 
    
    plot_ly(seasonal_sales, x = ~factor(month), y = ~factor(year), z = ~total_sales,
            type = "heatmap", colorscale = "Viridis") %>%
      layout(title = "Seasonal Patterns in Home Sales",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Year"))
  })
  
  output$monthly_sales_plot <- renderPlotly({
    monthly_sales <- filtered_data() %>%
      group_by(month, city) %>%
      summarise(avg_sales = mean(sales, na.rm = TRUE))
    
    plot_ly(monthly_sales, x = ~month, y = ~avg_sales, color = ~city, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Average Monthly Home Sales in Selected Cities",
             xaxis = list(title = "Month"),
             yaxis = list(title = "Average Sales"))
  })
  
  # City Comparison Tab
  output$city_comparison_plot <- renderPlotly({
    city_data <- filtered_data() %>%
      group_by(city) %>%
      summarise(
        median_price = median(median, na.rm = TRUE),
        avg_sales = mean(sales, na.rm = TRUE),
        avg_inventory = mean(inventory, na.rm = TRUE)
      )
    
    plot_ly(city_data, x = ~median_price, y = ~avg_sales, text = ~city, type = "scatter", mode = "markers",
            marker = list(size = ~avg_inventory, sizemode = "diameter", sizeref = 2, sizemin = 5),
            hoverinfo = "text", hovertext = ~paste(city, "<br>Median Price:", dollar(median_price),
                                                   "<br>Avg Sales:", round(avg_sales, 2),
                                                   "<br>Avg Inventory:", round(avg_inventory, 2))) %>%
      layout(title = "City Comparison: Price vs. Sales vs. Inventory",
             xaxis = list(title = "Median Price (USD)"),
             yaxis = list(title = "Average Sales"))
  })
  output$sales_trends_plot <- renderPlotly({
    sales_trends <- filtered_data() %>%
      group_by(year, month) %>%
      summarise(total_sales = sum(sales, na.rm = TRUE), .groups = "drop") %>%
      mutate(date = as.Date(paste(year, month, "01", sep = "-")))  # ✅ 创建 YYYY-MM 日期格式
    
    plot_ly(sales_trends, x = ~date, y = ~total_sales, type = "scatter", mode = "lines+markers",
            line = list(color = "green")) %>%
      layout(title = "Total Home Sales Over Time",
             xaxis = list(title = "Year-Month", tickformat = "%Y-%m"),
             yaxis = list(title = "Total Sales"))
  })
  
  output$city_comparison_map <- renderLeaflet({

    city_data <- filtered_data() %>%
      group_by(city) %>%
      summarise(median_price = median(median, na.rm = TRUE)) %>%
      inner_join(city_coords, by = "city")  
    city_data$scaled_size <- rescale(city_data$median_price, to = c(5, 15))
    leaflet(city_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat, 
        radius = ~scaled_size, 
        color = ~ifelse(median_price > 250000, "red", "blue"),  
        fillColor = ~ifelse(median_price > 250000, "red", "blue"),
        fillOpacity = 0.8, stroke = TRUE, weight = 1, opacity = 0.9,
        layerId = ~city,  
        label = ~paste0(city, ": $", format(median_price, big.mark = ",")), 
        popup = ~paste0("<b>", city, "</b><br>Median Price: $", format(median_price, big.mark = ",")), 
        options = markerOptions(opacity = 1, riseOnHover = TRUE) 
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("blue", "red"),
        labels = c("Low Price", "High Price"),
        title = "Median Home Price",
        opacity = 0.8
      ) %>%
      setView(lng = -99, lat = 31, zoom = 5) 
  })
  observeEvent(input$city_comparison_map_marker_click, {
    click <- input$city_comparison_map_marker_click
    if (!is.null(click)) {
      showNotification(paste0("You clicked on ", click$id), type = "message")
    }
  })
  output$inventory_price_plot <- renderPlotly({
    inventory_price <- filtered_data() %>%
      group_by(year, city) %>%
      summarise(
        avg_inventory = mean(inventory, na.rm = TRUE),
        median_price = median(median, na.rm = TRUE)
      )
    
    plot_ly(inventory_price, x = ~avg_inventory, y = ~median_price, color = ~city, text = ~year, type = "scatter", mode = "markers") %>%
      layout(title = "Inventory Levels vs. Median Prices in Selected Cities",
             xaxis = list(title = "Average Inventory"),
             yaxis = list(title = "Median Price (USD)"))
  })
  
  # Market Dynamics Tab
  # Market Dynamics - Total Sales Over Time
  output$sales_volume_plot <- renderPlotly({
    sales_volume <- filtered_data() %>%
      group_by(year) %>%
      summarise(total_sales = sum(sales, na.rm = TRUE))
    
    plot_ly(sales_volume, x = ~year, y = ~total_sales, type = "scatter", mode = "lines+markers",
            line = list(color = "orange")) %>%
      layout(title = "Total Home Sales Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Sales"))
  })
  
  # Market Dynamics - Total Volume Over Time
  output$total_volume_plot <- renderPlotly({
    sales_volume <- filtered_data() %>%
      group_by(year) %>%
      summarise(total_volume = sum(volume, na.rm = TRUE))
    
    plot_ly(sales_volume, x = ~year, y = ~total_volume, type = "scatter", mode = "lines+markers",
            line = list(color = "orange")) %>%
      layout(title = "Total Sales Volume Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Volume (USD)"))
  })
  
  output$inventory_trends_plot <- renderPlotly({
    inventory_trends <- filtered_data() %>%
      group_by(year, city) %>%
      summarise(avg_inventory = mean(inventory, na.rm = TRUE))
    
    plot_ly(inventory_trends, x = ~year, y = ~avg_inventory, color = ~city, type = "scatter", mode = "lines+markers") %>%
      layout(title = "Average Inventory Trends in Selected Cities",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Average Inventory"))
  })
  
  output$price_volume_bubble <- renderPlotly({
    price_volume <- filtered_data() %>%
      group_by(year, city) %>%
      summarise(
        median_price = median(median, na.rm = TRUE),
        total_volume = sum(volume, na.rm = TRUE),
        total_sales = sum(sales, na.rm = TRUE)
      )
    
    plot_ly(price_volume, x = ~median_price, y = ~total_volume, size = ~total_sales, color = ~city,
            text = ~paste("Year:", year, "<br>City:", city, "<br>Median Price:", dollar(median_price),
                          "<br>Total Volume:", dollar(total_volume), "<br>Total Sales:", total_sales),
            hoverinfo = "text", type = "scatter", mode = "markers") %>%
      layout(title = "Price vs. Volume vs. Sales Over Time in Selected Cities",
             xaxis = list(title = "Median Price (USD)"),
             yaxis = list(title = "Total Volume (USD)"))
  })
}

# Run the application
shinyApp(ui = ui, server=server)