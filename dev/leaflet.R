pacman::p_load(
  leaflet,
  sf,
  maps,
  dplyr
)

load('data/map_dat.rda')
dat <- map_dat



# Leaflet Map -------------------------------------------------------------

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))


# setView()
# fitBounds()
# clearBounds()

# Popup Content
content <- ~paste(
  "<div style='text-align: center;'>
  <b><a href='https://www.samurainoodle.com/'>",
  county_name,
  "</a></b></div>",
  "<strong>Land Area:</strong>", round(aland, 3), "<br>",
  "<strong>Water Area:</strong>", round(awater, 3), "<br>",
  "<strong>Hardship Index:</strong>", round(hardship, 3)
)

# Have to project into 4326
map_dat_proj <- st_transform(map_dat, 4326)

# Color palette
pal <- colorNumeric(
  palette = "YlGn",
  domain = map_dat$hardship,
  reverse = FALSE
)

# County Map
leaflet(map_dat_proj) %>%
  addProviderTiles(
    providers$Stadia.AlidadeSmooth, 
    group = 'Stadia AlidadeSmooth'
  ) %>%
  addProviderTiles(
    providers$CartoDB.Voyager, 
    group = 'CartoDB.Voyager'
  ) %>%
  addProviderTiles(
    providers$CartoDB.PositronNoLabels, 
    group = 'CartoDB.PositronNoLabels'
  ) %>%
  addProviderTiles(
    providers$CartoDB.Positron, 
    group = 'CartoDB.Positron'
  ) %>%
  addPolygons(
    color = "black",
    weight = 1, 
    smoothFactor = 0.5,
    opacity = 1.0, 
    fillOpacity = 0.8,
    fillColor = ~pal(map_dat$hardship),
    highlightOptions = highlightOptions(
      color = "white",
      weight = 2,
      bringToFront = TRUE
    ),
    popup = content,
    popupOptions = popupOptions(closeButton = FALSE),
    label = ~county_name,
    group = 'Counties'
  ) %>% 
  addLayersControl(
    baseGroups = c(
      'Stadia AlidadeSmooth',
      'CartoDB.Positron',
      'CartoDB.PositronNoLabels',
      'CartoDB.Voyager'
    ), 
    overlayGroups = c('Counties'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  # hideGroup('Counties') %>% 
  addLegend(
    "bottomright",
    pal = pal,
    values = ~ hardship,
    title = "Hardship Index",
    labFormat = labelFormat(prefix = ""),
    opacity = 1
  )

