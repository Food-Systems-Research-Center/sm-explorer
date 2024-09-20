#' TEST UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_TEST_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' TEST Server Functions
#'
#' @noRd 
mod_TEST_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_TEST_ui("TEST_1")
    
## To be copied in the server
# mod_TEST_server("TEST_1")
