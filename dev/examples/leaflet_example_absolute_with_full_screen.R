library(leaflet)
library(shiny)

js <- "
function openFullscreen(elem) {
  var map = $(elem).find('.leaflet.html-widget');
  if (elem.requestFullscreen) {
    elem.requestFullscreen();
  } else if (elem.mozRequestFullScreen) { /* Firefox */
    elem.mozRequestFullScreen();
  } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
    elem.webkitRequestFullscreen();
  } else if (elem.msRequestFullscreen) { /* IE/Edge */
    elem.msRequestFullscreen();
  }
  $(map).css('height', '100vh').trigger('resize');
}

document.addEventListener('fullscreenchange', exitHandler, false);
document.addEventListener('mozfullscreenchange', exitHandler, false);
document.addEventListener('MSFullscreenChange', exitHandler, false);
document.addEventListener('webkitfullscreenchange', exitHandler, false);

function exitHandler(){
  if (document.webkitIsFullScreen || document.mozFullScreen || document.msFullscreenElement) return;
  $('#map').css('height', '400px').trigger('resize');
}
 
"



ui <- fluidPage(
  actionButton(
    "fullscreen", "Full Screen Container",
    onclick = "openFullscreen(document.getElementById('map_container'))"
  ),
  div(
    id = "map_container",
    leafletOutput(height = "400px", "map"),
    absolutePanel(
      top = 20,
      right = 20,
      style = "color: #FFF",
      h1("test", style = "color:white"),
      selectInput("in2", "choose", 1:5),
      numericInput("in3", "number", 0, 0, 10)
    )
  ),
  tags$scrip(HTML(js))
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles('Esri.WorldImagery') %>% 
      setView(lng = 118.2437, lat = 34.0522, zoom = 5)
  })
}

shinyApp(ui, server)