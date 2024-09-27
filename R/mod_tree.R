#' tree UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import collapsibleTree
mod_tree_ui <- function(id) {
  ns <- NS(id)
  tagList(
    collapsibleTreeOutput(ns('tree'), height = '80vh', width = '100%')
  )
}
    
#' tree Server Functions
#'
#' @noRd 
mod_tree_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    load('data/tree_dat.rda')
    load('data/colors.rda')
    load('data/framework.rda')
    
    output$tree <- renderCollapsibleTree({
      levels <- c('Dimension', 'Index', 'Indicator')
      collapsibleTree(
        tree_dat, 
        levels,
        tooltip = TRUE,
        tooltipHTML = TRUE,
        nodeSize = 'count_',
        root = 'Sustainability Metrics',
        attribute = 'count_',
        fontSize = 14,
        linkLength = 400,
        fill = colors,
        inputId = 'tree'
      )
    })
    
  })
}
    
## To be copied in the UI
# mod_tree_ui("tree")
    
## To be copied in the server
# mod_tree_server("tree")
