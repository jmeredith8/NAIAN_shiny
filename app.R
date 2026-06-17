############## NAIAN Biomass Data Map Dashboard

### Setting working directory to folder in R drive
# setwd("R:/Ryan_Lab/Jakob_M/Projects/NAIAN_Dash")

### Loading in libraries
library(shiny)
library(leaflet)
library(sf)
library(raster)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra)
library(tidyverse) 
library(maps)
library(mapproj)
library(mapdata)
library(ggthemes)
library(maps)
library(ggplot2)
library(viridis)
library(viridisLite)
library(gridExtra)
library(ggspatial)
library(tigris)
library(ggpubr)
library(tidyterra)
library(paletteer)
library(classInt)
library(shinyWidgets)
library(rsconnect)
library(here)
library(treemap)
library(plotly)
library(scales)
library(classInt)

### Cleaning Data
# Load in global map and set projection to match points
world <- ne_countries(scale = "medium", returnclass = "sf")

world <- world %>% st_transform(4326)

# Load in locations of bug collections
locations <- read_csv(here("trap_data.csv"), locale = locale(encoding = "UTF-8")) %>%
  mutate(across(where(is.character), ~iconv(., from = "UTF-8", to = "UTF-8", sub = "")))

# Cleaning the latitude/longitude data so it reads nicely into the map
locations <- locations %>% 
  filter(!is.na(Latitude), !is.na(Long)) %>% 
  mutate(
    Long = as.numeric(gsub("[^0-9.-]", "", iconv(Long, "UTF-8", "ASCII", sub = ""))),
    Lat = as.numeric(gsub("[^0-9.-]", "", iconv(Latitude, "UTF-8", "ASCII", sub = "")))
  )

# Setting slight jitter to map points so they don't cover each other
locations <- locations %>% 
  mutate(
    lat_j = jitter(Lat, amount = 0.005),
    long_j = jitter(Long, amount = 0.005)
  )

# Filtering datapoints by those that have lat/long coordinates
locations %>% 
  filter(is.na(lat_j) | is.na(long_j) | !is.finite(lat_j) | !is.finite(long_j))

# Creating new variable with filtered datapoints with coordinates
plot_data <- locations %>%
  filter(!is.na(lat_j), !is.na(long_j))

# Renaming variables in the table
plot_data <- plot_data %>% 
  rename("state" = "State/Prov",
         "location" = "Town",
         "month" = "Month",
         "date" = "Start_Day",
         "pheno" = "Phenology",
         "region" = "Region",
         "alt" = "altitude",
         "temp" = "Mean_Temp",
         "wind" = "Mean_WIND",
         "precip" = "Mean_Precip",
         "sample_days" = "Days_of_ sampling",
         "unique_id" = "SortOrder")

# Testing formula for calculating the base value of biomass data
# tot_test = (((10^(plot_data$Log10_Nematocera_Day)) - 0.001) * (plot_data$sample_days))

# Calculating new column with base value biomass/day data (according to metadata)
plot_data <- plot_data %>%
  mutate(plot_data, tot_biomass = (((10^(plot_data$Log10_Tot_Biomass_Day)) - 0.001) * (plot_data$sample_days)))

plot_data <- plot_data %>% 
  mutate(tot_biomass = round(tot_biomass, 3),
         temp = round(temp, 2),
         wind = round(wind, 2),
         precip = round(precip, 2))

plot_data <- plot_data %>% 
  group_by(location, state, year) %>% 
  arrange(Date_1May, .by_group = TRUE) %>% 
  mutate(collection_num = row_number()) %>% 
  ungroup()

plot_data <- plot_data %>% 
  mutate(map_label = paste0(location, ", ", state))

plot_data <- plot_data %>% 
  mutate(region = gsub("^[0-9]+_", "", region))

# plot_data_agg <- plot_data %>% 
#   group_by(year, location, state) %>% 
#   summarise(
#     tot_biomass = round(mean(tot_biomass, na.rm = TRUE), 3),
#     temp = round(mean(temp, na.rm = TRUE), 2),
#     wind = round(mean(wind, na.rm = TRUE), 2),
#     precip = round(mean(precip, na.rm = TRUE), 2),
#     alt = mean(alt, na.rm = TRUE),
#     month = first(month),
#     date = first(date),
#     unique_id = first(unique_id),
#     long_j = mean(long_j),
#     lat_j = mean(lat_j),
#     .groups = "drop"
#   )

### Establishing UI
ui <- navbarPage("NAIAN Dashboard",
                 
                 header = tags$head(
                   tags$style(HTML("
                           #Make the sidebars look cleaner
                           .sidebar-header {
                             color: #2c3e50;
                             font-weight: bold;
                             margin-top: 20px;
                           }

                           # Style the main instructional text
                           .instruction_text {
                           font-size: 16px;
                           line-height: 1.6;
                           color: #444444;
                           background-color: #f9f9f9;
                           padding: 15px;
                           border-left: 5px solid #007bc2;
                           border-radius: 4px;
                           }

                           #Title text
                           .title-header {
                           text-align: center;
                           padding: 20px;
                           color: #2c3e50;
                           width: 100%;
                           }
                           
                           #hover_info {
                            margin-bottom: 10px;
                            padding: 10px 15px;
                            background-color: #f0f4f8;
                            border-left: 5px solid #007bc2;
                            border-radius: 4px;
                            font-size: 15px;
                            color: #2c3e50;
                            min-height: 40px;
                            }
                           "))
                 ),

                 tabPanel(
                   "Collections Map",
                   titlePanel("Collections Map"),
                   sidebarLayout(
                     sidebarPanel(
                       
                       p("Select a year of collections to view"),
                       
                       selectInput(
                         inputId = "year_select",
                         label = "Select a Year",
                         choices = c(
                           "Select a time..." = "",
                           "2019" = "2019",
                           "2020" = "2020",
                           "2021" = "2021"
                         ),
                         selected = ""
                      ),
                       
                       p("Select a variable to view a color-coded version of the map"),
                       
                       radioButtons(
                         inputId = "color_by",
                         label = "Color Markers By:",
                         choices = c(
                           "Month" = "month",
                           "Temperature" = "temp",
                           "Wind Speed" = "wind",
                           "Precipitation" = "precip",
                           "Altitude" = "alt",
                           "Biomass" = "tot_biomass", 
                           "Collection Number" = "collection_num"
                         ),
                         selected = "none"
                       ),
                       
                       p("Use the map to click on the markers for more details"),
                       
                       hr(),
                       
                       uiOutput("click_info")
                     ),
                     
                     
                     mainPanel(
                       leafletOutput("map", height = 700),
                       br(),
                       uiOutput("hover_biomass")
                     )
                   )
                 ),
                 
                 tabPanel(
                   "Trends Graphs",
                   titlePanel("Trends Graphs"),
                   sidebarLayout(
                     sidebarPanel(

                       p("Select a year of biomass trends to view for a specific site"),

                       selectInput(
                         inputId = "year_select",
                         label = "Select a Year",
                         choices = c(
                           "Select a time..." = "",
                           "2019" = "2019",
                           "2020" = "2020",
                           "2021" = "2021"
                         ),
                         selected = ""
                       ),
                       
                       conditionalPanel(
                         condition = "input.year_select != ''",
                         selectInput(
                           inputId = "site_select",
                           label = "Select a Site",
                           choices = NULL,
                           selected = NULL
                         )
                       ),

                     ),
                     
                    mainPanel(
                      fluidRow(
                        style = "margin-top: 50px;",
                        column(
                          width = 10, 
                          offset = 1, 
                          plotOutput("trend_chart", height = 600))
                      )
                    )
                   ),
                    
                    sidebarLayout(
                      sidebarPanel(
                        
                        p("Select a year of biomass trends to view for a specific site"),
                        
                        selectInput(
                          inputId = "year_select_quad",
                          label = "Select a Year",
                          choices = c(
                            "Select a time..." = "",
                            "2019" = "2019",
                            "2020" = "2020",
                            "2021" = "2021"
                          ),
                          selected = ""
                        ),
                        
                        radioButtons(
                          inputId = "charts_display",
                          label = "Compare Biomass with:",
                          choices = c(
                            "Temperature" = "temp",
                            "Wind Speed" = "wind",
                            "Precipitation" = "precip",
                            "Altitude" = "alt"
                          ),
                          selected = "none"
                        ),
                        
                      ),
                      
                      mainPanel(
                        fluidRow(
                          style = "margin-top: 50px;",
                          column(
                            width = 10, 
                            offset = 1, 
                            plotOutput("quad_chart", height = 600))
                        )
                      )
                     
                   )
                 )
)

### Establishing Server
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    color_by <- input$color_by
    
    filtered_data <- if(input$year_select == "") {
      plot_data
    } else {
      plot_data %>% filter(year == as.numeric(input$year_select))
    }
    
    n <- nrow(filtered_data)
    
    map <- leaflet(options = leafletOptions(preferCanvas = FALSE)) %>%
      addTiles() %>%
      setView(lng = -99.996, lat = 48.3689, zoom = 3)
    
    if (is.null(color_by) || color_by == "none" || n == 0) {
      return(
        map %>% addCircleMarkers(
          data = filtered_data,
          lng = ~long_j,
          lat = ~lat_j,
          radius = 4,
          color = "black",
          weight = 0.5,
          fillColor = "black", 
          fillOpacity = 0.8,
          stroke = TRUE,
          options = pathOptions(interactive = TRUE),
          layerId = ~as.character(unique_id),
          # layerId = ~paste0(lat_j, "_", long_j),
          label = lapply(paste0(
            "<b>Location:</b> ", filtered_data$map_label, "<br>",
            "<b>Year:</b> ", filtered_data$year),
            HTML)
        )
      )
    }
    
    values <- filtered_data[[color_by]]
    values[is.na(values) | trimws(values) == ""] <- "Unknown"
    
    unique_vals <- unique(values)
    
    if (color_by == "month") {
      pal <- colorFactor(palette = plasma(length(unique_vals)), domain = unique_vals)
    } else {
      numeric_vals <- as.numeric(values[values != "Unknown"])
      breaks <- classIntervals(numeric_vals, n = 5, style = "jenks")$brks
      pal <- colorBin(palette = plasma(5), domain = as.numeric(values), bins = breaks, na.color = "grey")
    }
    
    legend_title <- switch(color_by,
                           "year" = "Year",
                           "temp" = "Temperature (C)",
                           "wind" = "Wind Speed (km/hr)",
                           "precip" = "Precipitation (mm)",
                           "alt" = "Altitude (m)",
                           "tot_biomass" = "Biomass (g)",
                           "collection_num" = "Collection Number")

  map %>%
    addCircleMarkers(
      data = filtered_data,
      lng = ~long_j,
      lat = ~lat_j,
      radius = ~pmax(rescale(tot_biomass, to = c(2, 20)), 2),
      color = "black",
      weight = 2,
      fillColor = ~pal(filtered_data[[color_by]]), 
      fillOpacity = 0.8,
      stroke = TRUE,
      options = pathOptions(interactive = TRUE),
      layerId = ~as.character(unique_id),
      # layerId = ~paste0(lat_j, "_", long_j),
      label = lapply(paste0(
        "<b>Location:</b> ", filtered_data$map_label, "<br>",
        "<b>Date:</b> ", filtered_data$month, " ", filtered_data$date, ", ", filtered_data$year, "<br>",
        "<b>Collection:</b> ", filtered_data$collection_num),
        HTML)
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = values,
      title = legend_title,
      opacity = 1
    )

  })
  
hover_data <- reactiveVal(NULL)

observeEvent(input$map_marker_mouseover, {
  event <- input$map_marker_mouseover
  print(paste("SHAPE mouseover fired, id:", event$id))
  # Establishing source of hover text data
  if(!is.null(event$id)) {
    check_data <- plot_data[plot_data$unique_id == as.integer(event$id), ]
    print(paste("rows matched:", nrow(check_data)))
    if(nrow(check_data) > 0) {
      
      hover_data(as.data.frame(check_data))
    }
  }
  
})

observeEvent(input$map_marker_click, {
  click <- input$map_marker_click
  
  
  # # Identify the row by matching the lat/long of the clicked marker
  # row <- plot_data %>%
  #   filter(abs(lat_j - click$lat) < 0.01, abs(long_j - click$lng) < 0.01) %>%
  #   slice(1)
  # print(paste("rows found:", nrow(row)))
  # print(row)
  
  # Identify the row by matching the lat/long of the clicked marker
  row <- plot_data %>%
    filter(unique_id == as.integer(click$id))
  print(paste("rows found:", nrow(row)))
  print(row)
  
  output$click_info <- renderUI({
    print("inside renderUI")
    
    tryCatch({
      
      div(
        style = "padding: 15px; background-color: #f9f9f9; border-radius: 8px; border: 1px solid #ddd;",
        h3(paste0(row$map_label)),
        tags$table(
          style = "width: 100%; font-size: 16px;",
          tags$tr(
            tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Date Collected:"),
            tags$td(style = "padding: 5px;", row$month, " ", row$date, ", ", row$year)
          ),
          tags$tr(
            tags$td(style = "font-weight: bold; padding: 5px; width: 200px;", "Temperature:"),
            tags$td(style = "padding: 5px;", row$temp, " C")
          ),
          tags$tr(
            tags$td(style = "font-weight: bold; padding: 5px;", "Wind Speed:"),
            tags$td(style = "padding: 5px;", row$wind, " km/hr")
          ),
          tags$tr(
            tags$td(style = "font-weight: bold; padding: 5px;", "Precipitation:"),
            tags$td(style = "padding: 5px;", row$precip, " mm")
          ),
          tags$tr(
            tags$td(style = "font-weight: bold; padding: 5px;", "Altitude:"),
            tags$td(style = "padding: 5px;", row$alt, " m")
          ),
          tags$tr(
            tags$td(style = "font-weight: bold; padding: 5px;", "Total Biomass:"),
            tags$td(style = "padding: 5px;", row$tot_biomass, " g")
          ),
          tags$tr(
            tags$td(style = "font-weight: bold; padding: 5px;", "Collection Number:"),
            tags$td(style = "padding: 5px;", row$collection_num)
          ),
        )
      )
      
    }, error = function(e) {
      div(
        style = "padding: 15px; background-color: #fff3cd; border-radius: 8px; border: 1px solid #ffc107;",
        h4("Could not load details for this marker."),
        p(paste("Error:", e$message))
      )
    })
  })
})

observeEvent(input$year_select, {
  req(input$year_select != "")
  
  sites <- plot_data %>% 
    filter(year == as.numeric(input$year_select)) %>% 
    pull(map_label) %>% 
    unique() %>% 
    sort()
  
  updateSelectInput(session, "site_select", choices = sites, selected = sites[1])
})

output$trend_chart <- renderPlot({
  
  if (is.null(input$site_select) || input$site_select == "" ||
      is.null(input$year_select) || input$year_select == "") {
    return(
      ggplot() +
        labs(title = "Select a year and variable for comparing with biomass trends by region",
             x = "Day of Year", y = "Total Biomass") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, color = "grey60", size = 16))
    )
  }
  
  chart_data <- plot_data %>% 
    filter(map_label == input$site_select,
           year == as.numeric(input$year_select)) %>% 
    arrange(Date_1May)
  
  if (nrow(chart_data) == 0) {
    return(ggplot() + labs(title = "No data") + theme_void())
  }
  
  ggplot(chart_data, aes(x = factor(date), y = tot_biomass)) +
    geom_col(fill = "steelblue", color = "white") +
    geom_smooth(aes(x = as.numeric(factor(date))),
                method = "loess", se = FALSE, color = "orange", linewidth = 3) +
    labs(
      title = paste("Biomass Over Time - ", input$site_select, input$year_select),
      x = "Date (1 = May 1st)", 
      y = "Total Biomass (mg)") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 20)
    )
})

output$quad_chart <- renderPlot({
  x_var <- input$charts_display
  
  if (is.null(x_var) || x_var == "none") {
    return(
      ggplot() +
        labs(title = "Select a variable to compare with biomass") +
        theme_void() +
        theme(plot.title = element_text(hjust = 0.5, color = "grey60", size = 16))
    )
  }
  
  chart_data2 <- if (is.null(input$year_select_quad) || input$year_select_quad == "") {
    plot_data
  } else {
    plot_data %>% filter(year == as.numeric(input$year_select_quad))
  }
  
  if (nrow(chart_data2) == 0) {
    return(ggplot() + labs(title = "No data for this selection") + theme_void())
  }
  
  x_label <- switch(x_var,
                    "temp" = "Temperature (C)",
                    "wind" = "Wind Speed (km/hr)",
                    "precip" = "Precipitation (mm)",
                    "alt" = "Altitude (m)")
  
  ggplot(chart_data2, aes(x = .data[[x_var]], y = tot_biomass)) +
    geom_point(aes(color = region), alpha = 0.6, size = 2) +
    geom_smooth(method = "loess", se = TRUE, color = "black", linewidth = 0.8) +
    facet_wrap(~ region, scales = "free") +
    scale_color_manual(values = rainbow(length(unique(chart_data2$region)))) +
    labs(
      title = paste("Biomass vs ", x_label),
      x = x_label,
      y = "Total Biomass"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
      axis.title = element_text(size = 13),
      axis.text = element_text(size = 11),
      strip.text = element_text(face = "bold", size = 12),
      legend.position = "none"
    )
})

}

shinyApp(ui, server)



