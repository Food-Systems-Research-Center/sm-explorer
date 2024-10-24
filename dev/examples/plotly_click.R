pacman::p_load(
  dplyr,
  plotly,
  ggplot2,
  shiny,
  tibble
)

d <-  mtcars %>% 
  rownames_to_column("car")

# UI  ----
ui <- fluidPage(plotlyOutput("plot"),
                tableOutput("click"))

# server  ----
server <- function(input, output) {
  output$plot <- renderPlotly({
    
    key <- d$car
    
    p <- d %>% 
      ggplot(aes(x = disp, y = mpg, color = factor(cyl),
                 key = key)) +
      geom_point(size = 4, alpha = 0.7)
    
    ggplotly(p) %>% 
      event_register("plotly_click")
  })
  
  output$click <- renderTable({
    point <- event_data(event = "plotly_click", priority = "event")
    req(point) # to avoid error if no point is clicked
    filter(d, car == point$key) # use the key to find selected point
  })
}

# app ----
shinyApp(ui = ui, server = server)