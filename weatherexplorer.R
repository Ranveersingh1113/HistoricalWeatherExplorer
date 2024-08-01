# Load necessary libraries
library(shiny)
library(leaflet)
library(httr)
library(plotly)

# Define UI for application
ui <- fluidPage(
  titlePanel("Historical Weather Data Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h4("Select Location:"),
      verbatimTextOutput("selected_location"),
      verbatimTextOutput("temp_summary"),
      verbatimTextOutput("humidity_summary"),
      verbatimTextOutput("cloudiness_summary"),
      verbatimTextOutput("wind_speed_summary"),
      verbatimTextOutput("pressure_summary"),
      verbatimTextOutput("visibility_summary")
    ),
    mainPanel(
      leafletOutput("map"),
      tabsetPanel(
        tabPanel("Temperature Plot", 
                 plotlyOutput("temperature_plot", width = "100%", height = "500px")
        ),
        tabPanel("Humidity Plot",
                 plotlyOutput("humidity_plot", width = "100%", height = "500px")
        ),
        tabPanel("Cloudiness Plot",
                 plotlyOutput("cloudiness_plot", width = "100%", height = "500px")
        ),
        tabPanel("Wind Speed Plot",
                 plotlyOutput("wind_speed_plot", width = "100%", height = "500px")
        ),
        tabPanel("Atmospheric Pressure Plot",
                 plotlyOutput("pressure_plot", width = "100%", height = "500px")
        ),
        tabPanel("Visibility Plot",
                 plotlyOutput("visibility_plot", width = "100%", height = "500px")
        ),
        tabPanel("Weather Recommendation",
                 textOutput("weather_recommendation")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Initialize selected location
  selected_location <- reactiveVal(NULL)
  
  # Initialize map with default view
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -73.68988401705542, lat = 42.63496561409271, zoom = 10)  # Default view (Centered around Albany, NY)
  })
  
  # Update selected location
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lon <- click$lng
    selected_location(c(lat, lon))
  })
  
  # Display selected location
  output$selected_location <- renderPrint({
    location <- selected_location()
    if (!is.null(location)) {
      paste0("Selected Location: Lat = ", location[1], ", Lon = ", location[2])
    } else {
      "No location selected"
    }
  })
  
  # Function to fetch weather forecast data
  fetch_weather_forecast <- function(lat, lon, api_key) {
    url <- paste0("https://api.openweathermap.org/data/2.5/forecast?lat=", lat, "&lon=", lon, "&appid=", api_key)
    response <- httr::GET(url)
    
    if (httr::status_code(response) == 200) {
      data <- httr::content(response, "parsed")
      return(data)
    } else {
      cat("Error fetching weather forecast data:", httr::content(response, "text"), "\n")
      return(NULL)
    }
  }
  
  # Function to process forecast data and extract temperature
  process_forecast_data <- function(data) {
    temperatures <- sapply(data$list, function(x) {
      # Convert temperature from Kelvin to Fahrenheit
      temperature_kelvin <- x$main$temp
      temperature_fahrenheit <- ((temperature_kelvin - 273.15) * 9/5) + 32
      return(temperature_fahrenheit)
    })
    return(temperatures)
  }
  
  # Function to process forecast data and extract humidity
  process_humidity_data <- function(data) {
    humidity <- sapply(data$list, function(x) {
      return(x$main$humidity)
    })
    return(humidity)
  }
  
  # Function to process forecast data and extract cloudiness
  process_cloudiness_data <- function(data) {
    cloudiness <- sapply(data$list, function(x) {
      return(x$clouds$all)
    })
    return(cloudiness)
  }
  
  # Function to process forecast data and extract wind speed
  process_wind_speed_data <- function(data) {
    wind_speed <- sapply(data$list, function(x) {
      return(x$wind$speed)
    })
    return(wind_speed)
  }
  
  # Function to process forecast data and extract atmospheric pressure
  process_pressure_data <- function(data) {
    pressure <- sapply(data$list, function(x) {
      return(x$main$pressure)
    })
    return(pressure)
  }
  
  # Function to process forecast data and extract visibility
  process_visibility_data <- function(data) {
    visibility <- sapply(data$list, function(x) {
      return(x$visibility)
    })
    return(visibility)
  }
  
  # Function to provide weather recommendation based on forecasted conditions
  weather_recommendation <- function(temperatures, humidities, cloudiness, wind_speeds, pressures, visibilities) {
    # Determine personalized weather recommendation based on thresholds
    avg_temp <- mean(temperatures)
    avg_humidity <- mean(humidities)
    avg_cloudiness <- mean(cloudiness)
    avg_wind_speed <- mean(wind_speeds)
    avg_pressure <- mean(pressures)
    avg_visibility <- mean(visibilities)
    
    recommendation <- paste0("Based on the forecasted weather conditions:<br>")
    
    if (avg_temp > 75) {
      recommendation <- paste0(recommendation, "- It's going to be a hot day. Stay hydrated and wear sunscreen.<br>")
    } else if (avg_temp < 50) {
      recommendation <- paste0(recommendation, "- It's going to be cold. Dress warmly.<br>")
    }
    
    if (avg_humidity > 70) {
      recommendation <- paste0(recommendation, "- It's going to be humid. Stay cool and drink plenty of water.<br>")
    }
    
    if (avg_cloudiness > 50) {
      recommendation <- paste0(recommendation, "- It's going to be cloudy. Don't forget an umbrella.<br>")
    }
    
    if (avg_wind_speed > 10) {
      recommendation <- paste0(recommendation, "- Expect strong winds. Secure loose objects and be cautious.<br>")
    }
    
    if (avg_pressure < 1013) {
      recommendation <- paste0(recommendation, "- Low atmospheric pressure. Prepare for potential storms.<br>")
    }
    
    if (avg_visibility < 10000) {
      recommendation <- paste0(recommendation, "- Reduced visibility. Drive carefully and use headlights.<br>")
    }
    
    return(recommendation)
  }
  
  # Function to update plots and summary statistics based on map click
  observeEvent(input$map_click, {
    click <- input$map_click
    lat <- click$lat
    lon <- click$lng
    
    api_key <- "555814c377a5abb74259921f5e4630fc"  # Replace with your actual API key
    forecast_data <- fetch_weather_forecast(lat, lon, api_key)
    
    if (!is.null(forecast_data)) {
      temperatures <- process_forecast_data(forecast_data)
      output$temperature_plot <- renderPlotly({
        plot_ly(x = seq_along(temperatures), y = temperatures, type = 'scatter', mode = 'lines', line = list(color = 'blue')) %>%
          layout(title = list(text = "Temperature Forecast"),
                 xaxis = list(title = "Duration (hours)"),
                 yaxis = list(title = "Temperature (°F)"))
      })
      
      humidities <- process_humidity_data(forecast_data)
      output$humidity_plot <- renderPlotly({
        plot_ly(x = seq_along(humidities), y = humidities, type = 'scatter', mode = 'lines', line = list(color = 'green')) %>%
          layout(title = list(text = "Humidity Forecast"),
                 xaxis = list(title = "Duration (hours)"),
                 yaxis = list(title = "Humidity (%)"))
      })
      
      cloudiness <- process_cloudiness_data(forecast_data)
      output$cloudiness_plot <- renderPlotly({
        plot_ly(x = seq_along(cloudiness), y = cloudiness, type = 'scatter', mode = 'lines', line = list(color = 'gray')) %>%
          layout(title = list(text = "Cloudiness Forecast"),
                 xaxis = list(title = "Duration (hours)"),
                 yaxis = list(title = "Cloudiness (%)"))
      })
      
      wind_speeds <- process_wind_speed_data(forecast_data)
      output$wind_speed_plot <- renderPlotly({
        plot_ly(x = seq_along(wind_speeds), y = wind_speeds, type = 'scatter', mode = 'lines', line = list(color = 'orange')) %>%
          layout(title = list(text = "Wind Speed Forecast"),
                 xaxis = list(title = "Duration (hours)"),
                 yaxis = list(title = "Wind Speed (m/s)"))
      })
      
      pressures <- process_pressure_data(forecast_data)
      output$pressure_plot <- renderPlotly({
        plot_ly(x = seq_along(pressures), y = pressures, type = 'scatter', mode = 'lines', line = list(color = 'red')) %>%
          layout(title = list(text = "Atmospheric Pressure Forecast"),
                 xaxis = list(title = "Duration (hours)"),
                 yaxis = list(title = "Atmospheric Pressure (hPa)"))
      })
      
      visibilities <- process_visibility_data(forecast_data)
      output$visibility_plot <- renderPlotly({
        plot_ly(x = seq_along(visibilities), y = visibilities, type = 'scatter', mode = 'lines', line = list(color = 'purple')) %>%
          layout(title = list(text = "Visibility Forecast"),
                 xaxis = list(title = "Duration (hours)"),
                 yaxis = list(title = "Visibility (m)"))
      })
      
      # Provide weather recommendation
      output$weather_recommendation <- renderText({
        recommendation <- weather_recommendation(temperatures, humidities, cloudiness, wind_speeds, pressures, visibilities)
        return(recommendation)
      })
      
      # Update summary statistics
      output$temp_summary <- renderText({
        paste("Minimum:", min(temperatures), ", Maximum:", max(temperatures), 
              ", Mean:", round(mean(temperatures), 2), ", Median:", median(temperatures), 
              ", Std. Dev.:", round(sd(temperatures), 2))
      })
      
      output$humidity_summary <- renderText({
        paste("Minimum:", min(humidities), ", Maximum:", max(humidities), 
              ", Mean:", round(mean(humidities), 2), ", Median:", median(humidities), 
              ", Std. Dev.:", round(sd(humidities), 2))
      })
      
      output$cloudiness_summary <- renderText({
        paste("Minimum:", min(cloudiness), ", Maximum:", max(cloudiness), 
              ", Mean:", round(mean(cloudiness), 2), ", Median:", median(cloudiness), 
              ", Std. Dev.:", round(sd(cloudiness), 2))
      })
      
      output$wind_speed_summary <- renderText({
        paste("Minimum:", min(wind_speeds), ", Maximum:", max(wind_speeds), 
              ", Mean:", round(mean(wind_speeds), 2), ", Median:", median(wind_speeds), 
              ", Std. Dev.:", round(sd(wind_speeds), 2))
      })
      
      output$pressure_summary <- renderText({
        paste("Minimum:", min(pressures), ", Maximum:", max(pressures), 
              ", Mean:", round(mean(pressures), 2), ", Median:", median(pressures), 
              ", Std. Dev.:", round(sd(pressures), 2))
      })
      
      output$visibility_summary <- renderText({
        paste("Minimum:", min(visibilities), ", Maximum:", max(visibilities), 
              ", Mean:", round(mean(visibilities), 2), ", Median:", median(visibilities), 
              ", Std. Dev.:", round(sd(visibilities), 2))
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
