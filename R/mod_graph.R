#' graph UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import plotly
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stringr
source('R/filter_metrics.R')
source('R/data_pipeline_functions.R')
mod_graph_ui <- function(id) {
  ns <- NS(id)
  # tagList -----
  tagList(
    
    # Row: Inputs -----
    fluidRow(
      box(
        title = 'Select X-Axis Variable',
        width = 6,
        status = 'primary',
        solidHeader = TRUE,
        collapsible = TRUE,
        
        ## Search X -----
        selectizeInput(
          inputId = ns('search_x'),
          label = NULL,
          choices = dat$metric,
          selected = NULL,
          width = '100%',
          multiple = FALSE
        ),
        tags$head(
          tags$style(
            HTML(
              '
              .selectize-input {
                word-wrap: break-word;
                word-break: break-word;
                max-width: 100%; 
                  overflow: hidden; 
                  text-overflow: ellipsis; 
                  white-space: nowrap; 
              }
              
              .selectize-dropdown {
                word-wrap: break-word;
                word-break: break-word;
                max-width: 100px !important; 
                  overflow: hidden; 
                  text-overflow: ellipsis; 
              }
              '
            )
          )
        )
      ),
        
      box(
        title = 'Select X-Axis Variable',
        width = 6,
        status = 'primary',
        solidHeader = TRUE,
        collapsible = TRUE,
        
        ## Search Y -----
        selectizeInput(
          inputId = ns('search_y'),
          label = NULL,
          choices = dat$metric,
          selected = NULL,
          width = '100%',
          multiple = FALSE
        ),
        tags$head(tags$style(
          HTML(
            '
              .selectize-input {
                word-wrap: break-word;
                word-break: break-word;
                max-width: 100%; 
                  overflow: hidden; 
                  text-overflow: ellipsis; 
                  white-space: nowrap; 
              }
              
              .selectize-dropdown {
                word-wrap: break-word;
                word-break: break-word;
                max-width: 100px !important; 
                  overflow: hidden; 
                  text-overflow: ellipsis; 
              }
              '
          )
        ))
        
      )
            
      ),
      
      # Row: Buttons -----
      fluidRow(
        ## Action Buttons -----
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
      ),
    
    # Row: graph -----  
    fluidRow(
      ## plotlyOutput -----
      plotlyOutput(ns('graph'))
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
    load('data/aggregated_meta.rda')
    
    # Filter Dataset X -----
    observeEvent(input$dimension_x, {
      filtered <- dplyr::filter(dat, dimension == input$dimension_x)
      updateSelectInput(session, "index_x", choices = unique(filtered$index))
    })
    
    observeEvent(input$index_x, {
      filtered <- dplyr::filter(dat, index == input$index_x)
      updateSelectInput(session, "indicator_x", choices = unique(filtered$indicator))
    })
    
    observeEvent(input$indicator_x, {
      filtered <- dplyr::filter(dat, indicator == input$indicator_x)
      updateSelectInput(session, "metric_x", choices = unique(filtered$variable_name))
    })
    
    observeEvent(input$metric_x, {
      filtered <- dplyr::filter(dat, variable_name == input$metric_x)
      updateSelectInput(
        session, 
        "year_x", 
        choices = sort(unique(filtered$year), decreasing = TRUE)
      )
    })
    
    observeEvent(input$year_x, {
      filtered <- dplyr::filter(dat, variable_name == input$metric_x)
    })
    
    
    # Filter Dataset Y -----
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
    
    # Obs input$show_graph -----
    observeEvent(input$show_graph, {
      output$graph <- renderPlotly({
        
        load('data/dat.rda')
        
        ## Filter to selected variables -----
        dat <- dat %>% 
          unique() %>% 
          filter(metric %in% c(input$search_x, input$search_y)) 
        
        xvar <- unique(dat$variable_name[dat$metric == input$search_x])
        yvar <- unique(dat$variable_name[dat$metric == input$search_y])
        
        dat <- dat %>% 
          get_latest_year() %>% 
          mutate(
            variable_name = paste0(variable_name, '_', year),
            .keep = 'unused'
          ) %>% 
          pivot_wider(
            id_cols = c('fips', 'county_name', 'state_name'),
            names_from = 'variable_name',
            values_from = 'value'
          )
        
        # Reassign x and y vars after pasting year
        # Why are we doing this twice?
        xvar <- str_subset(names(dat), xvar)
        yvar <- str_subset(names(dat), yvar)

        # Plotly -----
        plot <- dat %>% 
          ggplot(aes(
            x = !!sym(xvar), 
            y = !!sym(yvar),
            key = county_name,
            text = paste0(
              '<b>', county_name, ', ', state_name, '</b>\n',
              xvar, ': ', format(round(!!sym(xvar), 3), big.mark = ','), '\n',
              yvar, ': ', format(round(!!sym(yvar), 3), big.mark = ',')
            )
          )) +
          geom_point(
            size = 2,
            alpha = 0.75
          ) +
          theme_classic()
        
        ggplotly(plot, tooltip = 'text') %>% 
          layout(
            hoverlabel = list(
              bgcolor = "#154732", 
              bordercolor = 'white',
              align = 'auto',
              font = list(
                size = 12,
                color = 'white'
              )
            )
          )
        
        # event_register(plotly_plot, 'plotly_click')
        
      })
    })
    
    # Output Click -----
    # output$click <- renderPrint({
    #   d <- event_data("plotly_click")
    #   if (is.null(d)) {
    #     "Click events appear here (double-click to clear)"
    #   } else {
    #     cat("Selected County: ", d$key)
    #   }
    # })
    
    
    
  })
}
    
## To be copied in the UI
# mod_graph_ui("graph")
    
## To be copied in the server
# mod_graph_server("graph")
