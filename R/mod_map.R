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
    leafletOutput(ns('map_plot'))
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
      mapview(map_dat, zcol = 'hardship')@map
    })
  })
}
    
## To be copied in the UI
# mod_map_ui("map_1")
    
## To be copied in the server
# mod_map_server("map_1")
