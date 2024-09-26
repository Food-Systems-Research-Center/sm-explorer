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
#' @import leaflet.extensions
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
      
      h2("Metric Explorer"),
      style = "z-index: 5001; background-color: rgba(255,255,255,0.8); 
        padding: 15px; border-radius: 8px; max-width: 300; 
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1);",
      
      selectInput(
        ns("dimension"), 
        "Select Dimension:",
        choices = c(
          'Environment' = 'environment',
          'Economics' = 'economics',
          'Production' = 'production',
          'Human' = 'human',
          'Health' = 'health'
        ),
        width = '100%'
      ),
      selectInput(
        ns("index"), 
        "Select Index:",
        choices = c(
          'Environment' = 'environment',
          'Economics' = 'economics',
          'Production' = 'production',
          'Human' = 'human',
          'Health' = 'health'
        ),
        width = '100%'
      ),
      selectInput(
        ns("var"), 
        "Select Metric:",
        choices = c(
          "Income per Operation" = "mean_farm_income_per_operation",
          "Labor Cost" = "hired_labor_expense_per_farm",
          "Total Expenses per Operation" = 'total_expenses_per_operation',
          "Number of Operations with Hired Labor" = "hired_labor_operations"
        ),
        selected = "mean_farm_income_per_operation",
        width = '100%'
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
      
      data('map_dat_proj')
      custom_popup <- ~paste0(
        "<div style='text-align: center;'>
        <b><a href='https://www.samurainoodle.com/'>",
        county_name,
        "</a></b></div>",
        "<strong>Land Area:</strong>", aland, "<br>",
        "<strong>Water Area:</strong>", awater, "<br>"
      )
      
      # Leaflet -----
      leaflet(map_dat_proj) %>% 
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
          fillOpacity = 0.8,
          fillColor = ~pal(map_dat$mean_farm_income_per_operation),
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
        addLegend(
          "bottomright",
          pal = pal,
          values = ~ mean_farm_income_per_operation,
          title = "Mean Income per Farm",
          labFormat = labelFormat(prefix = "$"),
          opacity = 1
        ) %>% 
        addFullscreenControl()
    })
      
    # Observe -----
    observe({
      selected_var <- input$var
      custom_popup <- ~paste0(
        "<div style='text-align: center;'>
        <b><a href='https://www.samurainoodle.com/'>",
        county_name,
        "</a></b></div>",
        "<strong>Land Area:</strong>", aland, "<br>",
        "<strong>Water Area:</strong>", awater, "<br>"
      )
      leafletProxy(
        ns("map_plot"), 
        data = map_dat_proj
      ) %>%
        clearGroup('Counties') %>%
        addPolygons(
          color = "black",
          weight = 1, 
          smoothFactor = 0.5,
          opacity = 1.0, 
          fillOpacity = 0.8,
          fillColor = ~pal(map_dat_proj[[input$var]]),
          highlightOptions = highlightOptions(
            color = "white",
            weight = 2,
            bringToFront = TRUE
          ),
          popup = ~custom_popup,
          popupOptions = popupOptions(closeButton = FALSE),
          label = ~county_name,
          group = 'Counties'
        )
    })
    
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_1")
    
## To be copied in the server
# mod_map_server("map_1")
