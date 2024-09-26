
# Housekeeping ------------------------------------------------------------


pacman::p_load(
  leaflet,
  sf,
  maps,
  dplyr,
  mapview,
  tidyr,
  leaflet.extras
)

load('data/map_dat.rda')
dat <- map_dat
dat <- readRDS('dev/ne_economics.RDS') %>% 
  pivot_wider(names_from = 'variable_name')  
counties <- readRDS('dev/counties.RDS')

map_dat <- inner_join(counties, dat, by = 'fips')


# Leaflet Map -------------------------------------------------------------

# Set value for the minZoom and maxZoom settings.
leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18))


# setView()
# fitBounds()
# clearBounds()

# Have to project into 4326
map_dat_proj <- st_transform(map_dat, 4326) %>%
  mutate(across(c(aland, awater), 
                ~ format(round(. / 1000000, 1), big.mark = ',') %>% 
                  paste('sq km')))
  
# Popup Content
content <- ~paste(
  "<div style='text-align: center;'>
  <b><a href='https://www.samurainoodle.com/'>",
  county_name,
  "</a></b></div>",
  "<strong>Land Area:</strong>", aland, "<br>",
  "<strong>Water Area:</strong>", awater, "<br>",
  "<strong>Hardship Index:</strong>", round(mean_farm_income_per_operation, 2)
)

# Color palette
pal <- colorNumeric(
  palette = "YlGn",
  domain = map_dat$mean_farm_income_per_operation,
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
  addProviderTiles(
    providers$Stadia.StamenTerrain, 
    group = 'Stadia.StamenTerrain'
  ) %>%
  addProviderTiles(
    providers$Stadia.Outdoors, 
    group = 'Stadia.Outdoors'
  ) %>%
  addProviderTiles(
    providers$Stadia.StamenWatercolor, 
    group = 'Stadia.StamenWatercolor'
  ) %>%
  addProviderTiles(
    providers$USGS.USImageryTopo, 
    group = 'USGS.USImageryTopo'
  ) %>%
  addProviderTiles(
    providers$USGS.USImagery, 
    group = 'USGS.USImagery'
  ) %>%
  addPolygons(
    color = "black",
    weight = 1, 
    smoothFactor = 0.5,
    opacity = 1.0, 
    fillOpacity = 0.8,
    fillColor = ~pal(map_dat$mean_farm_income_per_operation),
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
      'CartoDB.Voyager',
      'Stadia.StamenTerrain',
      'Stadia.Outdoors',
      'Stadia.StamenWatercolor',
      'USGS.USImageryTopo',
      'USGS.USImagery'
    ), 
    overlayGroups = c('Counties'),
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  # hideGroup('Counties') %>% 
  addLegend(
    "bottomright",
    pal = pal,
    values = ~ mean_farm_income_per_operation,
    title = "Mean Income per Farm",
    labFormat = labelFormat(prefix = "$"),
    opacity = 1
  ) %>% 
  addFullscreenControl()

