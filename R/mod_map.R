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
mod_map_ui <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns('map_plot'), height = '600px') # width = '800px', height = '600px')
  )
}
    
#' map Server Functions
#'
#' @noRd 
mod_map_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    output$map_plot <- renderLeaflet({
      
      data('map_dat')
      custom_popup <- paste0(
        "<b>County:</b> ", map_dat$county, 
        "<br><b>Food Hardship:</b> ", round(map_dat$hardship, 3)
      )
      
      mapview(
        map_dat, 
        zcol = 'hardship',
        label = 'county',
        layer.name = 'Food Hardship Index',
        popup = custom_popup
      )@map
    })
  })
}
    
## To be copied in the UI
# mod_map_ui("map_1")
    
## To be copied in the server
# mod_map_server("map_1")
