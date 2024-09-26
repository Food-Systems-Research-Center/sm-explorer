#' table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import DT
mod_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns('table'), width = '300px')
  )
}
    
#' table Server Functions
#'
#' @noRd 
mod_table_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    data(dat)
    output$table <- DT::renderDT({
      datatable(
        dat,
        filter = 'top',
        style = 'bootstrap'
        # extensions = 'Buttons'
      )
    })
 
  })
}
    
## To be copied in the UI
# mod_table_ui("table_1")
    
## To be copied in the server
# mod_table_server("table_1")
