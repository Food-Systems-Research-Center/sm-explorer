library(shiny)
library(leaflet)
library(DT)

##Setup##
mapdata <- quakes
mapdata$latitude <- as.numeric(mapdata$lat)
mapdata$longitude <- as.numeric(mapdata$long)
mapdata$id <- 1:nrow(mapdata)

ui <- fluidPage(
  leafletOutput("mymap")
)

server <- function(input, output, session) {
  
  observeEvent(input$button_click, {
    
    clicked_point <- reactiveVal(input$mymap_marker_click)
    showModal(modalDialog(
      title = "More Details",
      renderDataTable({
        df <- mapdata[mapdata['id'] == clicked_point()$id,]
        x <- as.data.frame(cbind(colnames(df),t(df)),row.names = F);colnames(x) <- c("Field","Value")
        x
      })
    ))
  })
  
  output$mymap <- renderLeaflet({
    leaflet(options = leafletOptions(maxZoom = 18)) %>% addTiles() %>%
      addMarkers(lat = ~ latitude, lng = ~ longitude,
                 data = mapdata,
                 layerId = mapdata$id,
                 popup= ~paste("<b>", mag, "</b></br>", actionLink(inputId = "modal", label = "View Details", onclick = 'Shiny.setInputValue(\"button_click\", this.id, {priority: \"event\"})')))
  })
}

shinyApp(ui, server)