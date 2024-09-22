#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    dashboardPage(
      dashboardHeader(title = "SMExplorer"),
      dashboardSidebar(
        sidebarMenu(
          id = 'tabs',
          menuItem("Random Graph 1", tabName = "random_tab_1", icon = icon("dashboard")),
          # div(style = "margin: 10px;", actionButton("create_graph_1", "Create Graph 1")),
          conditionalPanel(
            condition = "input.tabs == 'random_tab_1'",
            div(style = "margin: 10px;", 
                actionButton("create_graph_1", "Hello There")
            )
          ),
          menuItem("Random Graph 2", tabName = "random_tab_2", icon = icon("dashboard")),
          conditionalPanel(
            condition = "input.tabs == 'random_tab_2'",
            div(style = "margin: 10px;", 
                actionButton("create_graph_2", "Hello There")
            )
          )
        )
      ),
      dashboardBody(
        tabItems(
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
