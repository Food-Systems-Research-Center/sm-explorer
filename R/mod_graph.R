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
    
    fluidRow(
      
      ## plotlyOutput -----
      box(
        width = 7,
        plotlyOutput(ns('graph'))
      ),
      
      box(
        title = 'Select Metrics',
        width = 5,
        status = 'primary',
        solidHeader = TRUE,
        collapsible = TRUE,
        
        ## Search X -----
        selectizeInput(
          inputId = ns('search_x'),
          label = 'Select X-Axis:',
          choices = dat$metric,
          selected = NULL,
          width = '100%',
          multiple = FALSE
        ),
        
        # format search x button
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
        ),
        
        ## Search Y -----
        selectizeInput(
          inputId = ns('search_y'),
          label = 'Select Y-Axis:',
          choices = dat$metric,
          selected = NULL,
          width = '100%',
          multiple = FALSE
        ),
        
        # Format search y button
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
        )),
        
        ## LOESS -----
        # tags$style(HTML("
        #   .custom-checkbox .icheckbox_square-green {
        #     background-color: #f0f0f0;
        #     border-color: #28a745;
        #   }
        #   .custom-checkbox .icheckbox_square-green.checked {
        #     background-color: #28a745;
        #     border-color: #28a745;
        #   }
        #   .custom-checkbox .awesome-checkbox-label {
        #     color: #28a745;
        #   }
        # ")),
        
        awesomeCheckbox(
          inputId = ns("loess"),
          label = "Add LOESS Curve", 
          value = FALSE,
          status = "primary"
        ),
        
        tags$style(HTML(paste0(
          "#", ns("loess"), " { ",
          "background-color: #154734 !important; ",
          "color: white !important; ",
          "width: 50%; ",
          "margin-left: auto; ",
          "margin-right: auto; ",
          "display: block; ",
          "} "
        ))),
        
        ## Action Buttons -----
        actionBttn(
          ns('show_graph'),
          'Make Graph',
          block = TRUE,
          style = 'jelly',
          color = 'primary'
        ),
        
        # format show graph button
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
}
    
#' graph Server Functions
#'
#' @noRd 
mod_graph_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Obs input$show_graph -----
    observeEvent(input$show_graph, {
      output$graph <- renderPlotly({
        browser
        load('data/dat.rda')
        
        ## Filter to selected variables -----
        dat <- dat %>% 
          # unique() %>% 
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
        # Because otherwise we can't link to the variable with _year suffix
        xvar <- str_subset(names(dat), xvar)
        yvar <- str_subset(names(dat), yvar)

        # Plotly -----
        plot <- dat %>% 
          ggplot(aes(
            x = !!sym(xvar), 
            y = !!sym(yvar),
            # key = county_name,
            text = paste0(
              '<b>', county_name, ', ', state_name, '</b>\n',
              xvar, ': ', format(round(!!sym(xvar), 3), big.mark = ','), '\n',
              yvar, ': ', format(round(!!sym(yvar), 3), big.mark = ',')
            )
          )) +
          geom_point(
            size = 2.5,
            alpha = 0.65
          ) +
          # [] add labels here, split up old two step process of xvar yvar
          # and use the first set to yoink the metrics (axis names) here
          theme_classic()
        
        # Add smoother curve if selected
        if (input$loess == TRUE) {
          plot <- plot + 
            geom_smooth(aes(group = 1))
            # geom_smooth(aes(x = !!sym(xvar), y = !!sym(yvar)), method = "loess")
        }
          
        ggplotly(plot, tooltip = 'text') %>% 
          layout(
            hoverlabel = list(
              bgcolor = "#154732", 
              bordercolor = 'white',
              align = 'auto',
              font = list(
                size = 14,
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
