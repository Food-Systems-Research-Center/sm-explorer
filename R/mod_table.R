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
#' @import dplyr
#' @import reactable
#' @import stringr
#' @import htmltools
mod_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    reactable::reactableOutput(ns('metrics_table'))
  )
}
    
#' table Server Functions
#'
#' @noRd 
mod_table_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Load full metadata table
    load('data/aggregated_meta.rda')
    
    # Pick out variables to display
    metadata <- aggregated_meta %>% 
      select(
        metric,
        definition,
        dimension,
        index,
        indicator,
        units,
        year = latest_year,
        source,
        scope,
        resolution,
        url
      )
    
    # Fix capitalization of column names
    names(metadata) <- str_to_title(names(metadata))

    # Reactable -----
    
    output$metrics_table <- reactable::renderReactable({
      reactable(
        metadata,
        sortable = TRUE,
        resizable = TRUE,
        filterable = TRUE,
        searchable = TRUE,
        pagination = TRUE,
        bordered = TRUE,
        wrap = TRUE,
        rownames = FALSE,
        onClick = 'select',
        striped = TRUE,
        pageSizeOptions = c(5, 10, 25, 50, 100),
        defaultPageSize = 5,
        showPageSizeOptions = TRUE,
        highlight = TRUE,
        style = list(fontSize = "14px"),
        compact = TRUE,
        columns = list(
          # Dimension = colDef(
          # minWidth = 75,
          # sticky = 'left'
          # ),
          # Index = colDef(
          # minWidth = 75,
          # sticky = 'left'
          # ),
          # Indicator = colDef(
          # minWidth = 100,
          # sticky = 'left'
          # ),
          Metric = colDef(
            minWidth = 200,
            sticky = 'left'
          ),
          Definition = colDef(
            minWidth = 250,
          ),
          # Units = colDef(minWidth = 50),
          Year = colDef(minWidth = 75),
          Source = colDef(minWidth = 250),
          Scope = colDef(show = FALSE),
          Resolution = colDef(show = FALSE),
          Url = colDef(
            minWidth = 300,
            show = FALSE
          )
        ),
        defaultColDef = colDef(minWidth = 100),
        # elementId = "metrics_table",
        details = function(index) {
          div(
            style = "padding: 15px; border: 1px solid #ddd; margin: 10px 0;
             background-color: #E0EEEE; border-radius: 10px; border-color: black;
             box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);",
            
            tags$h4(
              strong("Details"),
            ),
            tags$p(
              strong('Metric Name: '),
              as.character(metadata[index, 'Metric']),
            ),
            tags$p(
              strong('Definition: '),
              as.character(metadata[index, 'Definition']),
            ),
            tags$p(
              strong('Source: '),
              as.character(metadata[index, 'Source'])
            ),
            tags$p(
              strong('Latest Year: '),
              as.character(metadata[index, 'Year'])
            ),
            # tags$p(
            #   strong('Updates: '),
            #   str_to_title(as.character(metadata[index, 'Updates']))
            # ),
            tags$p(
              strong('URL: '),
              tags$a(
                href = as.character(metadata[index, 'Url']),
                target = '_blank',
                as.character(metadata[index, 'Url'])
              )
            )
          )
        }
      )
    })
    
    
    # output$metrics_table <- reactable::renderReactable({
    #   tagList(
    #     tags$div(
    #       style = "display: flex; gap: 16px; margin-bottom: 20px; justify-content: center;",
    # 
    #       tags$button(
    #         class = "btn btn-primary",
    #         style = "display: flex; align-items: center; gap: 8px; padding: 8px 12px;",
    #         tagList(fontawesome::fa("download"), "Show/hide more columns"),
    #         onclick = "Reactable.setHiddenColumns('metrics_table', prevColumns => {
    #       return prevColumns.length === 0 ? ['Definition', 'Scope', 'Resolution', 'Url'] : []
    #     })"
    #       ),
    # 
    #       tags$button(
    #         class = "btn btn-primary",
    #         style = "display: flex; align-items: center; gap: 8px; padding: 8px 12px;",
    #         tagList(fontawesome::fa("download"), "Download as CSV"),
    #         onclick = "Reactable.downloadDataCSV('metrics_table', 'sustainability_metrics.csv')"
    #       )
    #     ),
    # 
    #     reactable(
    #       metadata,
    #       sortable = TRUE,
    #       resizable = TRUE,
    #       filterable = TRUE,
    #       searchable = TRUE,
    #       pagination = TRUE,
    #       bordered = TRUE,
    #       wrap = TRUE,
    #       rownames = FALSE,
    #       onClick = 'select',
    #       striped = TRUE,
    #       pageSizeOptions = c(5, 10, 25, 50, 100),
    #       defaultPageSize = 5,
    #       showPageSizeOptions = TRUE,
    #       highlight = TRUE,
    #       style = list(fontSize = "14px"),
    #       compact = TRUE,
    #       columns = list(
    #         # Dimension = colDef(
    #         # minWidth = 75,
    #         # sticky = 'left'
    #         # ),
    #         # Index = colDef(
    #         # minWidth = 75,
    #         # sticky = 'left'
    #         # ),
    #         # Indicator = colDef(
    #         # minWidth = 100,
    #         # sticky = 'left'
    #         # ),
    #         Metric = colDef(
    #           minWidth = 200,
    #           sticky = 'left'
    #         ),
    #         Definition = colDef(
    #           minWidth = 250,
    #         ),
    #         # Units = colDef(minWidth = 50),
    #         Year = colDef(minWidth = 75),
    #         Source = colDef(minWidth = 250),
    #         Scope = colDef(show = FALSE),
    #         Resolution = colDef(show = FALSE),
    #         Url = colDef(
    #           minWidth = 300,
    #           show = FALSE
    #         )
    #       ),
    #       defaultColDef = colDef(minWidth = 100),
    #       elementId = "metrics_table",
          # details = function(index) {
          #   div(
          #     style = "padding: 15px; border: 1px solid #ddd; margin: 10px 0;
          #    background-color: #E0EEEE; border-radius: 10px; border-color: black;
          #    box-shadow: 2px 2px 10px rgba(0, 0, 0, 0.1);",
          # 
          #     tags$h4(
          #       strong("Details"),
          #     ),
          #     tags$p(
          #       strong('Metric Name: '),
          #       as.character(metadata[index, 'Metric']),
          #     ),
          #     tags$p(
          #       strong('Definition: '),
          #       as.character(metadata[index, 'Definition']),
          #     ),
          #     tags$p(
          #       strong('Source: '),
          #       as.character(metadata[index, 'Source'])
          #     ),
          #     tags$p(
          #       strong('Latest Year: '),
          #       as.character(metadata[index, 'Year'])
          #     ),
          #     # tags$p(
          #     #   strong('Updates: '),
          #     #   str_to_title(as.character(metadata[index, 'updates']))
          #     # ),
          #     tags$p(
          #       strong('URL: '),
          #       tags$a(
          #         href = as.character(metadata[index, 'Url']),
          #         target = '_blank',
          #         as.character(metadata[index, 'Url'])
          #       )
          #     )
          #   )
          # }
    #     )
    #   )
    # })
  })
}
    
## To be copied in the UI
# mod_table_ui("table")
    
## To be copied in the server
# mod_table_server("table")
