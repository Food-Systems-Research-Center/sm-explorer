load('data/dat.rda')
dat$variable_name %>% 
  unique %>% 
  sort

test <- dat %>% 
  filter(variable_name %in% c('groc', 'grocpth')) %>% 
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

get_str(test)

xvar <- str_subset(names(test), 'groc_')
yvar <- str_subset(names(test), 'grocpth_')

test %>% 
  ggplot(aes(x = groc_2016, y = grocpth_2016)) + 
  geom_point()

# Plotly -----
plot <- test %>% 
  ggplot(aes(
    x = !!sym(xvar),
    y = !!sym(yvar),
    text = paste0(
      county_name, '\n',
      state_name, '\n',
      '<b>', xvar, '</b>: ', format(round(!!sym(xvar), 3), big.mark = ','), '\n',
      '<b>', yvar, '</b>: ', format(round(!!sym(yvar), 3), big.mark = ',')
    )
  )) +
  geom_point() +
  theme_classic()
plot

ggplotly(plot, tooltip = 'text') %>% 
  layout(
    hovertemplate = paste(
      "<b>Centered Tooltip:</b><br>",  # Custom content
      "X: %{x}<br>",
      "Y: %{y}<br>",
      "Label: %{text}<extra></extra>"  # Removes default trace info
    ),
    hoverlabel = list(bgcolor = "white", font = list(size = 12))
  )


ggplotly(plot) %>% 
  layout(
    hovermode = "closest",
    hoverlabel = list(bgcolor = "white", font = list(size = 12)),
    hovertemplate = paste(
      "X: %{x}<br>",
      "Y: %{y}<br>",
      "Label: %{text}<extra></extra>"
    )
  )
  
  

# Plotly ------------------------------------------------------------------

get_str(test)
xvar
yvar

test %>%
  plot_ly(
    x = ~ test[[xvar]],
    y = ~ test[[yvar]],
    hoverinfo = 'text',
    text = ~ paste(
      xvar,
      test[[xvar]],
      '<br>',
      yvar,
      test[[yvar]]
    )
  ) %>%
  add_markers() %>% 
  layout(
    hoverlabel = list(bgcolor = "white", font = list(size = 12))
  )


test %>%
  plot_ly(
    x = ~ test[[xvar]],
    y = ~ test[[yvar]],
    hoverinfo = 'text',
    text = ~ paste(
      'NA_Sales:', test[[xvar]],  # Use dynamic values directly
      '<br>',
      'EU_Sales:', test[[yvar]]
    )
  ) %>%
  add_markers() %>% 
  layout(
    hovermode = "closest",  # To ensure the hover is closest to points
    hoverlabel = list(
      bgcolor = "white",   # Customize the tooltip's appearance
      font = list(size = 12)
    ),
    hovertemplate = paste(
      "X: %{x}<br>",     # Use Plotly's syntax: %{x} for x values
      "Y: %{y}<br>",     # Use %{y} for y values
      "Label: %{text}<extra></extra>"  # %{text} for the hover text
    )
  )
