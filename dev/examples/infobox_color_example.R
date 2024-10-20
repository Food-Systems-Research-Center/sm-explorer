require(shiny)
require(shinydashboard)

ui <- shinyUI(dashboardPage(
  dashboardHeader(title = 'Change infoBox color'),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$style(
      type = 'text/css', 
      '.bg-aqua {background-color: #005CB9!important; }'
    ),
    
    infoBox(
      title = 'Custom Color',
      value = 100,
      color = 'aqua'
    )
  )
))

server <- shinyServer(function(input, output) {
  
})

shinyApp(ui, server)