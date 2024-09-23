#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_map_server('map_plot')
  mod_table_server('table')
  mod_ipsum_graph_server("random_graph_1", reactive(input$create_graph_1))
  mod_ipsum_graph_2_server("random_graph_2", reactive(input$create_graph_2))
}
