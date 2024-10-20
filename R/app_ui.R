#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
#' 
source('R/my_theme.R')
app_ui <- function(request) {
  tagList(
    dashboardPage(
      dashboardHeader(
        title = "SMExplorer"
      ),
      
      # Sidebar -----------------------------------------------------------
      dashboardSidebar(
        sidebarMenu(
          id = 'tabs',
          menuItem("Interactive Map", tabName = "map_tab", icon = icon("map")),
          menuItem("Graph Explorer", tabName = "graph_tab", icon = icon("chart-simple")),
          # conditionalPanel(
          #   condition = "input.tabs == 'graph_tab'",
          #   div(
          #     style = "text-align: center; margin-bottom: 10px; width: 100%; white-space: normal; overflow-wrap: break-word;",
          #     HTML("<p>Choose variables to explore using the options below.</p>")
          #   ),
          #   div(
          #     style = "display: flex; justify-content: center; align-items: center; height: 50px;",
          #     selectInput("select_x_var", "Choose x variable")
          #   )
          #   # div(
          #   #   style = "display: flex; justify-content: center; align-items: center; height: 50px;",
          #   #   actionButton("show_graph", "Create Graphs")
          #   # )
          # ),
          menuItem("Metrics Framework", tabName = "tree_tab", icon = icon("sitemap")),
          menuItem("Data Explorer", tabName = "table_tab", icon = icon("table"))
        )
      ),
      
      # Body ---------------------------------------------------------------
      dashboardBody(
        # Formatting from my_theme.R CSS
        my_theme,
        tabItems(
          tabItem(
            tabName = 'map_tab',
            h2(strong('Sustainability Metrics Explorer')),
            mod_map_ui('map_plot')
          ),
          tabItem(
            tabName = 'graph_tab',
            h2(strong('Graph Explorer')),
            mod_graph_ui("graph")
          ),
          tabItem(
            tabName = 'tree_tab',
            h2(strong('Metrics Framework')),
            mod_tree_ui('tree')
          ),
          tabItem(
            tabName = 'table_tab',
            h2(strong('Data Explorer')),
            mod_table_ui('table')
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www", 
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SMexplorer"
    )
  )
}
