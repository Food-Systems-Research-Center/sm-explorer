#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
#' 
app_ui <- function(request) {
  tagList(
    dashboardPage(
      dashboardHeader(title = "SMExplorer"),
      
      # Sidebar -----------------------------------------------------------
      dashboardSidebar(
        sidebarMenu(
          id = 'tabs',
          menuItem("Interactive Map", tabName = "map_tab", icon = icon("map")),
          menuItem("Data Explorer", tabName = "table_tab", icon = icon("table")),
          menuItem("Random Graph 1", tabName = "random_tab_1", icon = icon("dashboard")),
          conditionalPanel(
            condition = "input.tabs == 'random_tab_1'",
            div(
              style = "text-align: center; margin-bottom: 10px; width: 100%; white-space: normal; overflow-wrap: break-word;",
              HTML(
                "<p>Testing some HTML text. fherul erghue ghrukl gherklug
                erhgkludfh g. fhlweuf efh woeufh weoufh ewwef ouwehf weouf
                f weruffh uher fweuhf wkleuhf wekluf.</p>"
              )
            ),
            div(
              style = "display: flex; justify-content: center; align-items: center; height: 50px;",
              actionButton("create_graph_1", "Hello There")
            )
          ),
          menuItem("Random Graph 2", tabName = "random_tab_2", icon = icon("dashboard")),
          conditionalPanel(
            condition = "input.tabs == 'random_tab_2'",
            div(
              style = "display: flex; justify-content: center; align-items: center; height: 50px;",
              actionButton("create_graph_2", "General Kenobi")
            )
          )
        )
      ),
      
      # Body ---------------------------------------------------------------
      dashboardBody(
        tabItems(
          tabItem(
            tabName = 'map_tab',
            h2(strong('Food Hardship Index 2020')),
            mod_map_ui('map_plot')
          ),
          tabItem(
            tabName = 'table_tab',
            h2(strong('Data Explorer')),
            mod_table_ui('table')
          ),
          tabItem(tabName = "random_tab_1",
                  h2('The First Random Graph'),
                  mod_ipsum_graph_ui("random_graph_1")
          ),
          tabItem(tabName = "random_tab_2",
                  h2('The Second Random Graph'),
                  mod_ipsum_graph_2_ui("random_graph_2")
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
