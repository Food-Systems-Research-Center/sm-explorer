#' graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
source('R/filter_metrics.R')
mod_graph_ui <- function(id) {
  ns <- NS(id)
  # tagList -----
  tagList(
    fluidRow(
      box(plotlyOutput(ns('graph')), width = 6),
      
      box(
        fluidRow(
          # Choose x axis -----
          box(
            title = 'Choose X-Axis',
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            
            selectInput(
              ns("dimension_x"),
              "Select Dimension:",
              choices = unique(dat$dimension),
              selected = NULL,
              width = '100%'
            ),
            selectInput(
              ns("index_x"),
              "Select Index:",
              choices = NULL,
              width = '100%'
            ),
            selectInput(
              ns("indicator_x"),
              "Select Indicator:",
              choices = NULL,
              width = '100%'
            ),
            selectInput(
              ns("metric_x"),
              "Select Metric:",
              choices = NULL,
              width = '100%'
            ),
            selectInput(
              ns("year_x"),
              "Select Year:",
              choices = NULL,
              width = '100%'
            )
          ),
          
          # Choose y axis -----
          box(
            title = 'Choose Y-Axis',
            status = 'primary',
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 6,
            
            # Filter buttons
            selectInput(
              ns("dimension_y"), 
              "Select Dimension:",
              choices = unique(dat$dimension),
              selected = NULL,
              width = '100%'
            ),
            selectInput(
              ns("index_y"), 
              "Select Index:",
              choices = NULL,
              width = '100%'
            ),
            selectInput(
              ns("indicator_y"), 
              "Select Indicator:",
              choices = NULL,
              width = '100%'
            ),
            selectInput(
              ns("metric_y"), 
              "Select Metric:",
              choices = NULL,
              width = '100%'
            ),
            selectInput(
              ns("year_y"), 
              "Select Year:",
              choices = NULL,
              width = '100%'
            )
          )
        ),
        fluidRow(
          actionBttn(
            ns('show_graph'),
            'Make Graph',
            block = TRUE,
            style = 'jelly',
            color = 'primary'
          ),
          tags$style(HTML(paste0(
            "#", ns("show_graph"), " { ",
            "background-color: #154734 !important; ",
            "color: white !important; ",
            "width: 50%; ",
            "margin-left: auto; ",
            "margin-right: auto; ",
            "display: block; ",
            "} "
          )))
        )
      )
    )
  )
}
    
#' graph Server Functions
#'
#' @noRd 
mod_graph_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Prep -----
    load('data/dat.rda')
    load('data/counties_2021.rda')
    load('data/counties_2024.rda')
    
    # Filter Dataset -----
    observeEvent(input$dimension_y, {
      filtered <- dplyr::filter(dat, dimension == input$dimension_y)
      updateSelectInput(session, "index_y", choices = unique(filtered$index))
    })
    
    observeEvent(input$index_y, {
      filtered <- dplyr::filter(dat, index == input$index_y)
      updateSelectInput(session, "indicator_y", choices = unique(filtered$indicator))
    })
    
    observeEvent(input$indicator_y, {
      filtered <- dplyr::filter(dat, indicator == input$indicator_y)
      updateSelectInput(session, "metric_y", choices = unique(filtered$variable_name))
    })
    
    observeEvent(input$metric_y, {
      filtered <- dplyr::filter(dat, variable_name == input$metric_y)
      updateSelectInput(
        session, 
        "year_y", 
        choices = sort(unique(filtered$year), decreasing = TRUE)
      )
    })
    
    observeEvent(input$year_y, {
      filtered <- dplyr::filter(dat, variable_name == input$metric_y)
    })
    
    # Show graph -----
    observeEvent(input$show_graph, {
      output$graph <- renderPlotly({
        # browser()
        plot <- dat %>% 
          filter(
            variable_name == input$metric_y,
            year == input$year_y
          ) %>% 
          ggplot(aes(x = value)) +
          geom_histogram(
            fill = 'grey',
            color = 'black'
          ) + 
          theme_classic()
        ggplotly(plot)
        
      })
    })
    
  })
}
    
## To be copied in the UI
# mod_graph_ui("graph")
    
## To be copied in the server
# mod_graph_server("graph")
