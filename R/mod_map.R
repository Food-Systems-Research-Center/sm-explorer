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
#' @import dplyr
#' @import shinycssloaders
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # tags$style(HTML(
    #   ".centered-button {
    #     display: flex;
    #     justify-content: center;
    #     width: 50%;
    #   }
    #   .custom-action-button {
    #     width: 200px;
    #   }"
    # )),
    
    div(
      id = 'map_container',
      
      # Leaflet output -----
      withSpinner(
        type = 6,
        color = '#154734',
        caption = HTML('Loading Map...'),
        leafletOutput(ns('map_plot'), height = '90vh', width = '100%')
      ),
      
      # Absolute Panel -----
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
        
        # Search metric -----
        selectizeInput(
          inputId = ns('metric'),
          label = 'Select Metric:',
          choices = NULL,
          selected = NULL,
          width = '100%',
          multiple = FALSE
        ),
        
        selectInput(
          inputId = ns("year"),
          label = "Select Year:",
          choices = NULL,
          selected = NULL,
          width = '100%'
        ),
        
        # Update button -----
        actionBttn(
          ns('update_map'),
          'Update Map',
          block = TRUE,
          style = 'jelly',
          color = 'primary',
          icon = icon('arrows-rotate')
        ),
        
        tags$style(HTML(paste0(
          "#", ns("update_map"), " { ",
          "background-color: #154734 !important; ",
          "color: white !important; ",
          "} "
        ))),
        
        # Gap between buttons
        HTML("<div style='height: 20px;'></div>"),
        
        # Fullscreen Button -----
        actionBttn(
          ns('full_screen'),
          'Full Screen',
          block = TRUE,
          style = 'jelly',
          color = 'primary',
          icon = icon('expand'),
          onclick = "openFullscreen(document.getElementById('map_container'))"
        ),
        
        tags$style(HTML(paste0(
          "#", ns("full_screen"), " { ",
          "background-color: #154734 !important; ",
          "color: white !important; ",
          "} "
        ))),
      
    ), # end div
    
    # JS function for full screen button
    tags$scrip(HTML(js))
      
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
      load('data/dat.rda')
      load('data/counties_2021.rda')
      load('data/counties_2024.rda')
      load('data/js.rda')
      load('data/sm_data.rda')
      source('R/filter_fips.R')
      
      # Narrow down list of potential metric options
      # Note this is a cluster and I need to fix it. 
      metric_options <- sm_data$metrics %>%
          inner_join(sm_data$metadata, by = 'variable_name') %>% 
          pull(metric) %>% 
          unique()
      
      updateSelectInput(
        session, 
        'metric', 
        choices = metric_options
      )
      
      # Baseline data to make map
      init_data <- sm_data$ne_counties_2024 %>% 
        left_join(sm_data$fips_key, by = 'fips')
      
      # Prep popup and palette
      custom_popup <- ~paste0(
        "<div style='text-align: center;'><b>", county_name, "</b></div>",
        "<strong>Land Area:</strong> ", round(aland / 1000000, 1), " sq km<br>",
        "<strong>Water Area:</strong> ", round(awater / 1000000, 1), " sq km<br>"
      )
      # county_palette <- colorFactor(
      #   "viridis",
      #   initial_dat$county_name
      # )
      
      
      # centroids <- st_centroid(initial_dat)
      # centroid_coords <- data.frame(
      #   county_name = centroids$county_name,
      #   lng = st_coordinates(centroids)[, 1],
      #   lat = st_coordinates(centroids)[, 2]
      # )
      
      # Initial Map -----
      leaflet(init_data) %>% 
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
          fillOpacity = 0.5,
          # fillColor = ~county_palette(initial_dat$county_name),
          fillColor = 'lightgray',
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
        # addLabelOnlyMarkers(
        #   data = centroid_coords,
        #   label = ~county_name,
        #   lng = ~lng,
        #   lat = ~lat,
        #   labelOptions = labelOptions(
        #     noHide = TRUE, 
        #     direction = "auto", 
        #     textsize = "10px",
        #     textOnly = TRUE
        #   )
        # ) %>%
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
        )
        # addFullscreenControl()
    })
    
    
    # Update year field -----
    observeEvent(input$metric, {
      req(input$metric)
      year_options <- sm_data$metadata %>%
        dplyr::filter(metric == input$metric) %>% 
        pull(year) %>% 
        str_split_1(', ') %>% 
        sort(decreasing = TRUE)

      updateSelectInput(
        session,
        "year",
        choices = year_options
      )
    })
    
    
    # Update Map (Prep) -----
    observeEvent(input$update_map, {
      req(input$metric, input$year)
      
      # Get corresponding variable_name
      chosen_variable <- sm_data$metadata %>% 
        filter(metric == input$metric) %>% 
        pull(variable_name)
      
      # Filter dataset based on user choices
      # Also join to metadata to get axis names and metric names
      updated_dat <- sm_data$metrics %>% 
        dplyr::filter(
          variable_name == chosen_variable,
          year == input$year
        ) %>% 
        mutate(value = as.numeric(value)) %>% 
        left_join(sm_data$metadata, by = 'variable_name')
      
      # Get resolution of metric
      res <- sm_data$metadata %>% 
        filter(variable_name == chosen_variable) %>% 
        pull(resolution)
      
      # Join with counties or states depending on resolution
      # Also choose county map depending on year (CT Discrancies)
      if (res == 'county') {
        if (input$year >= 2023) {
          updated_dat <- updated_dat %>%
            dplyr::right_join(sm_data$ne_counties_2024, by = 'fips')
        } else if (input$year <= 2022) {
          updated_dat <- updated_dat %>%
            dplyr::right_join(sm_data$ne_counties_2021, by = 'fips')
        }
      } else if (res == 'state') {
        updated_dat <- updated_dat %>% 
          dplyr::right_join(sm_data$ne_states_2024, by = 'fips')
      }
      
      # Add county name
      updated_dat <- updated_dat %>% 
        left_join(sm_data$fips_key, by = 'fips')
      
      # Popups and palette
      custom_popup <- function(county_name, 
                               variable_name,
                               value) {
        paste0(
          "<div style='text-align: center;'>",
          "<b>", county_name, "</b><br>",
          "<strong>", variable_name, ":</strong> ", round(value, 2)
        )
      }
      pal <- colorNumeric(
        palette = "YlGn",
        domain = updated_dat$value,
        reverse = FALSE
      )
      
      # Make sure updated_dat is an sf object after joins
      updated_dat <- st_as_sf(updated_dat)

      
      # LeafletProxy -----
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
          popup = ~custom_popup(county_name, variable_name, value),
          popupOptions = popupOptions(closeButton = FALSE),
          label = ~county_name,
          group = 'Counties'
        ) %>% 
        clearControls() %>% 
        addLegend(
          "bottomleft",
          pal = pal,
          values = ~value,
          title = ~axis_name[1],
          labFormat = labelFormat(prefix = " "),
          # labFormat = labelFormat(prefix = "$"),
          opacity = 1
        )
        # addFullscreenControl()
    })
    
  })
}
    
## To be copied in the UI
# mod_map_ui("map_1")
    
## To be copied in the server
# mod_map_server("map_1")
