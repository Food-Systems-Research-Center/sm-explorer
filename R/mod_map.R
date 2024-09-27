#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' 
#' @import leaflet
#' @import mapview
#' @import sf
#' @import leaflet.extras
#' @import shinyWidgets
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns('map_plot'), height = '80vh', width = '100%'),
    absolutePanel(
      id = "controls",
      class = "panel panel-default",
      fixed = TRUE,
      draggable = TRUE,
      top = 150,
      left = "auto",
      right = 20,
      bottom = "auto",
      width = 300,
      height = "auto",
      
      h2('Select Metrics', style = 'text-align: center; font-weight: bold;'),
      style = "z-index: 5001; background-color: rgba(255,255,255,0.8); 
        padding: 15px; border-radius: 8px; max-width: 300; 
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);",
      
      # Filter buttons
      selectInput(
        ns("dimension"), 
        "Select Dimension:",
        choices = unique(dat$dimension),
        selected = NULL,
        width = '100%'
      ),
      selectInput(
        ns("index"), 
        "Select Index:",
        choices = NULL,
        width = '100%'
      ),
      selectInput(
        ns("indicator"), 
        "Select Indicator:",
        choices = NULL,
        width = '100%'
      ),
      selectInput(
        ns("metric"), 
        "Select Metric:",
        choices = NULL,
        width = '100%'
      ),
      selectInput(
        ns("year"), 
        "Select Year:",
        choices = NULL,
        width = '100%'
      ),
      actionBttn(
        ns('update_map'),
        'Update Map',
        block = TRUE,
        style = 'jelly',
        color = 'success'
      )
    )
  )
}
    
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$map_plot <- renderLeaflet({
      
      # Prep -----
      data('dat')
      data('counties_2021')
      data('counties_2024')
      
      # Initial filter with dat
      initial_dat <- dat %>% 
        filter(variable_name == 'local_sales_pct')
      
      # Make spatial object with initial_dat
      initial_dat <- counties_2021 %>% 
        inner_join(initial_dat, by = 'fips')
      
      # Prep popup and palette
      custom_popup <- ~paste0(
        "<div style='text-align: center;'>
        <b><a href='https://www.samurainoodle.com/'>",
        county_name,
        "</a></b></div>",
        "<strong>Land Area:</strong> ", round(aland / 1000000, 1), " sq km<br>",
        "<strong>Water Area:</strong> ", round(awater / 1000000, 1), " sq km<br>"
      )
      county_palette <- colorFactor(
        "viridis",
        initial_dat$county_name
      )
      
      # Initial Map -----
      leaflet(initial_dat) %>% 
        addProviderTiles(
          providers$Stadia.AlidadeSmooth, 
          group = 'Stadia AlidadeSmooth'
        ) %>%
        addProviderTiles(
          providers$Stadia.StamenTerrain, 
          group = 'Stadia.StamenTerrain'
        ) %>%
        addProviderTiles(
          providers$Stadia.Outdoors, 
          group = 'Stadia.Outdoors'
        ) %>%
        addProviderTiles(
          providers$Stadia.StamenWatercolor, 
          group = 'Stadia.StamenWatercolor'
        ) %>%
        addProviderTiles(
          providers$USGS.USImagery, 
          group = 'USGS.USImagery'
        ) %>% 
        addPolygons(
          color = "black",
          weight = 1, 
          smoothFactor = 0.5,
          opacity = 1.0, 
          fillOpacity = 0.6,
          fillColor = ~county_palette(initial_dat$county_name),
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          popup = custom_popup,
          popupOptions = popupOptions(closeButton = FALSE),
          label = ~county_name,
          group = 'Counties'
        ) %>% 
        addLayersControl(
          baseGroups = c(
            'Stadia AlidadeSmooth',
            'Stadia.StamenTerrain',
            'Stadia.StamenWatercolor',
            'Stadia.Outdoors',
            'USGS.USImagery'
          ), 
          overlayGroups = c('Counties'),
          options = layersControlOptions(collapsed = TRUE),
          position = 'topleft'
        ) %>% 
        addFullscreenControl()
    })
    
    
    # Filter Dataset -----
    observeEvent(input$dimension, {
      filtered <- filter(dat, dimension == input$dimension)
      updateSelectInput(session, "index", choices = unique(filtered$index))
    })
    
    observeEvent(input$index, {
      filtered <- filter(dat, index == input$index)
      updateSelectInput(session, "indicator", choices = unique(filtered$indicator))
    })
    
    observeEvent(input$indicator, {
      filtered <- filter(dat, indicator == input$indicator)
      updateSelectInput(session, "metric", choices = unique(filtered$variable_name))
    })
    
    observeEvent(input$metric, {
      filtered <- filter(dat, variable_name == input$metric)
      updateSelectInput(
        session, 
        "year", 
        choices = sort(unique(filtered$year), decreasing = TRUE)
      )
    })
    
    # observeEvent(input$year, {
    #   filtered <- filter(dat, variable_name == input$metric)
    #   updateSelectInput(session, "year", choices = unique(filtered$year))
    # })
    
    
    # Update Map -----
    observeEvent(input$update_map, {
      req(input$dimension, input$index, input$indicator, input$metric, input$year)
      
      # Filter dataset based on user choices
      updated_dat <- dat %>% 
        filter(
          dimension == input$dimension,
          index == input$index,
          indicator == input$indicator,
          variable_name == input$metric,
          year == input$year
        )
      
      # Join with counties
      updated_dat <- counties_2021 %>% 
        inner_join(updated_dat, by = 'fips')
      
      # Popups and palette
      custom_popup <- function(county_name, 
                               aland, 
                               awater, 
                               variable_name,
                               value) {
        paste0(
          "<div style='text-align: center;'>",
          "<b><a href='https://www.samurainoodle.com/'>", county_name, "</a></b><br>",
          "<strong>Land Area:</strong> ", round(aland / 1000000, 2), " sq km<br>",
          "<strong>Water Area:</strong> ", round(awater / 100000, 2), " sq km<br>",
          "<strong>", variable_name, ":</strong> ", round(value, 2)
        )
      }
      pal <- colorNumeric(
        palette = "YlGn",
        domain = updated_dat$value,
        reverse = FALSE
      )

      leafletProxy(
        ns("map_plot"), 
        data = updated_dat
      ) %>%
        clearGroup('Counties') %>%
        addPolygons(
          color = "black",
          weight = 1, 
          smoothFactor = 0.5,
          opacity = 1.0, 
          fillOpacity = 0.75,
          fillColor = ~pal(updated_dat$value),
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          popup = ~custom_popup(county_name, aland, awater, variable_name, value),
          popupOptions = popupOptions(closeButton = FALSE),
          label = ~county_name,
          group = 'Counties'
        ) %>% 
        clearControls() %>% 
        addLegend(
          "bottomleft",
          pal = pal,
          values = ~value,
          title = ~variable_name[1],
          labFormat = labelFormat(prefix = "$"),
          opacity = 1
        ) %>%
        addFullscreenControl()
    })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_1")
    
## To be copied in the server
# mod_map_server("map_1")
