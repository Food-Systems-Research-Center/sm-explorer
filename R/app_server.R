#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_map_server('map_plot')
  mod_graph_server('graph')
  mod_table_server('table')
  mod_tree_server("tree")
}
