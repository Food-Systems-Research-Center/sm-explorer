#' DataCamp - Interactive Data Visualization with Plotly
#' 2024-05-31

pacman::p_load(plotly)


# Chapter 1 - What is Plotly ----------------------------------------------


#' Vis library
#' viewer windows, r markdown documents, shiny apps
#' active development with a supportive community

# Load the plotly package
library(plotly)
# Plotly loads dplyr and ggplot2

# Store the scatterplot of Critic_Score vs. NA_Sales sales in 2016
scatter <-  vgsales %>%
  filter(Year == 2016) %>%
  ggplot(aes(x = NA_Sales, y = Critic_Score)) +
  geom_point(alpha = 0.3)

# Convert the scatterplot to a plotly graphic
ggplotly(scatter)


#' Not all ggplot objects can be converted into plotly
#'  and there are more options if you do it manually in plotly

###
# Create a histogram of Critic_Score with at most 25 bins
vgsales %>%
  plot_ly(x = ~ Critic_Score) %>%
  add_histogram(nbinsx = 25)

# Create a histogram with bins of width 10 between 0 and 100
vgsales %>%
  plot_ly(x = ~ Critic_Score) %>%
  add_histogram(xbins = list(
    start = 0,
    end = 100,
    size = 10))

# Create a frequency for Genre
genre_table <- vgsales %>%
  count(Genre)

# Reorder the bars for Genre by n
genre_table %>%
  mutate(Genre = fct_reorder(Genre, n, .desc = TRUE)) %>%
  plot_ly(x = ~Genre, y = ~n) %>% 
  add_bars()                      
# NOTE: the function n, which only works within dplyr pipes, functions


### Bivariate Graphics
#' add_markers is a scatter plot
#' layout(barmode = 'stack') for stacked bar charts
#' 
#' Boxplots
#' add_boxplot()

# Create a scatter plot of User_Score against Critic_Score
vgsales %>% 
  plot_ly(x = ~ Critic_Score, y = ~ User_Score) %>%
  add_markers()

# Filter out the 2016 video games
vg2016 <- vgsales %>%
  filter(Year == 2016)

# Create a stacked bar chart of Rating by Genre
vg2016 %>%
  count(Genre, Rating) %>%
  plot_ly(x = ~ Genre, y = ~ n, color = ~ Rating) %>%
  add_bars() %>%
  layout(barmode = 'stack')

# Create boxplots of Global_Sales by Genre for above data
vg2016 %>% 
  plot_ly(x = ~ Global_Sales, y = ~ Genre) %>%
  add_boxplot()



# Chapter 2: Customizing Traces -------------------------------------------


# Color
add_histogram(color = I('red')) # Note I argument.

# Alpha
add_markers(marker = list(opacity = 0.2))

# Symbols
add_markers(marker = list(symbol = 'circle-open'))
# Note that this marker list is hinky. Is it necessary with just one arg?

# size, width, other arguments


###
# Create a histogram of Critic_Score with navy bars that are 50% transparent
vgsales2016 %>%
  plot_ly(x = ~Critic_Score) %>%
  add_histogram(color = I('navy'), opacity = 0.5)

# Change the color of the histogram using a hex code
vgsales2016 %>%
  plot_ly(x = ~Critic_Score) %>%
  add_histogram(color = I('#111e6c'))

# Change the color of the histogram using rgb()
vgsales2016 %>%
  plot_ly(x = ~Critic_Score) %>%
  add_histogram(marker = list(color = 'rgb(17, 30, 108)'))
# NOTE: I do not like this marker list scenario. But I think it was also in
# flextable and lavaanPlot. Maybe even reactable?
# Fourth argument in rgb() controls the opacity too

# Set the plotting symbol to diamond and the size to 4
plot_ly(data = vg2016, x = ~User_Score, y = ~Critic_Score) %>% 
  add_markers(marker = list(
    symbol = 'diamond', size = 4)) 


###' Thoughtful use of color
#' Default colors based on whether data is factor or continuous
add_markers(colors = 'Dark2')
add_markers(colors = c('orange', 'black', 'skyblue'))

# Use color to add Genre as a third variable
vgsales2016 %>%
  plot_ly(x = ~Critic_Score, 
          y = ~User_Score,
          color = ~Genre) %>%
  add_markers(colors = 'Dark2')
# WHY is there no marker and list here?

# Create a scatterplot of User_Score against Critic_Score coded by Rating
vgsales2016 %>%
  plot_ly(x = ~Critic_Score, 
          y = ~User_Score,
          symbol = ~Rating) %>%
  add_markers()
# Automatically colors them differently too

# Create a scatterplot of User_Score vs. Critic_Score colored by log User_Count
vgsales2016 %>%
  plot_ly(x = ~Critic_Score, 
          y = ~User_Score,
          color = ~log(User_Count)) %>%
  add_markers()
# NOTE: transforming values to make the color scheme work better


### Labeling Data
plot_ly(x, y, hoverinfo = 'all') # default. Can do x, y, x+y+z
hoverinfo = 'text' # to manually define what to show
text = ~paste('Label:', variable, '<br>', 'Label:', variable)
# NOTE the <br> is an html line break

# Create a bar chart of Platform with hoverinfo only for the bar heights
vgsales2016 %>%
  count(Platform) %>%
  plot_ly(x = ~Platform, y = ~n, hoverinfo = "y") %>%
  add_bars()
# I hate the counts and ns
# Note that by specifying y, the hoverinfo only shows the y value, which is n

# Create a scatterplot of User_Score vs. Critic score
vgsales2016 %>%
  # Add video game Name to the hover info text
  plot_ly(
    x = ~Critic_Score, 
    y = ~User_Score,
    hoverinfo = 'text',
    text = ~Name
  ) %>% 
  add_markers()

# Format the hover info for NA_Sales, EU_Sales, and Name
vgsales2016 %>%
  plot_ly(x = ~NA_Sales, y = ~EU_Sales,
          hoverinfo = 'text',
          text = ~paste(
            'NA_Sales:', NA_Sales, '<br>',
            'EU_Sales:', EU_Sales, '<br>',
            'Name:', Name)) %>%
  add_markers()
# Noice. This is what we've been doing. Validation.


### Customizing the Layout
layout() # is like labs and theme combined
# This takes more weird lists
layout(xaxis = list(title = 'fredsff', type = 'log', zeroline = FALSE),
       yaxis = list(title = 'wefwef', type = 'log', showgrid = FALSE),
       title = 'sefsef',
       plot_bgcolor = toRGB('gray90'),
       paper_bgcolor = toRGB('skyblue'))
# NOTE: axes can take more options. The list lets us set many at once
# Note showgrid and zeroline
# plot color is plot background
# Paper is the dead space around the graph
# Note that toRGB() converts R colors into RGB for plotly

# Polish the scatterplot by transforming the x-axis and labeling both axes
vgsales2016 %>%
  plot_ly(x = ~Global_Sales, y = ~Critic_Score) %>%
  add_markers(marker = list(opacity = 0.5)) %>%
  layout(xaxis = list(title = "Global sales (millions of units)", 
                      type = 'log'),
         yaxis = list(title = 'Critic score'))

# Set the background color to #ebebeb and remove the vertical grid
annual_vgsales %>%
  plot_ly(x = ~Year, y = ~Global_Sales) %>%
  add_lines() %>%
  layout(xaxis = list(showgrid = FALSE),
         paper_bgcolor = '#ebebeb')



# Chapter 3: Layering Traces ----------------------------------------------


# Adding a smoother. First make model separately. Why is this even necessary
m <- loess(alcohol ~ flavanoids, data = wine, span = 1.5)

# Then put it in model
add_lines(y = ~fitted(m), name = 'LOESS') %>% 
add_markers(showlegend = FALSE) # otherwise we end up with two legends

# Layering densities (this is a crazy pain in the ass)
# Each one has to be done independently??
d1 <- filter(wine, Type == 1)
density1 <- density(d1$Flavanoids)
add_lines(x = density1$x, y = ~density1$y, name = 'Type 1')
# Repeat for each density curve
# I do not like this one bit

###
# Fit the regression model of User_Score on Critic_Score
m <- lm(User_Score ~ Critic_Score, data = vgsales2016)

# Create the scatterplot with smoother
vgsales2016 %>%
  select(User_Score, Critic_Score) %>%
  na.omit() %>%
  plot_ly(x = ~Critic_Score, y = ~User_Score) %>%
  add_markers(showlegend = FALSE) %>%
  add_lines(y = ~fitted(m))

# Compute density curves
d.a <- density(activision$Critic_Score, na.rm = TRUE)
d.e <- density(ea$Critic_Score, na.rm = TRUE)
d.n <- density(nintendo$Critic_Score, na.rm = TRUE)

# Overlay density plots
plot_ly() %>%
  add_lines(x = ~d.a$x, y = ~d.a$y, name = "Activision", fill = 'tozeroy') %>%
  add_lines(x = ~d.e$x, y = ~d.e$y, name = "Electronic Arts", fill = 'tozeroy') %>%
  add_lines(x = ~d.n$x, y = ~d.n$y, name = "Nintendo", fill = 'tozeroy') %>%
  layout(xaxis = list(title = 'Critic Score'),
         yaxis = list(title = 'Density'))
# This is literally the worst
# NOTE: tozeroy just fills in the area under the curve


### Facets and Subplots
# can do it manually:
subplot(p1, p2, nrows = 1, shareY = TRUE, shareX = TRUE)
# Note that you should add a name to markers so it is informative

# Automate it
vgsales %>% 
  group_by(Genre) %>% 
  nest() %>% 
  mutate(plot = map2(data, Genre, \(data, Genre){
    plot_ly...
  })) %>% 
  subplot(nrows = 2)


###
# Create a scatterplot of User_Score against Critic_Score for PS4 games
p1 <- vgsales2016 %>%
  filter(Platform == "PS4") %>%
  plot_ly(x = ~Critic_Score, y = ~User_Score) %>% 
  add_markers(name = "PS4")

# Create a scatterplot of User_Score against Critic_Score for XOne games
p2 <- vgsales2016 %>%
  filter(Platform == "XOne") %>%
  plot_ly(x = ~Critic_Score, y = ~User_Score) %>% 
  add_markers(name = "XOne")

# Create a facted scatterplot containing p1 and p2
subplot(p1, p2, nrows = 2)

# Create a faceted scatterplot of User_Score vs. Critic_Score with 3 rows
vgsales2016 %>%
  group_by(Platform) %>%
  nest() %>%
  mutate(
    plot = map2(
      data, Platform,
      \(data, Platform) 
      plot_ly(data = data, x = ~Critic_Score, y = ~User_Score) %>%
        add_markers(name = ~Platform)
    )) %>%
  subplot(nrows = 3, shareY = TRUE, shareX = TRUE)
# NOTE: nest() creates an object called data that you can use later

# Add x-axis and y-axis labels, and a title
subplot(p1, p2, nrows = 2, shareX = TRUE, shareY = TRUE) %>%
  layout(title = "User score vs. critic score by platform, 2016")
# If we don't share, something about titles will get fucked up because defaults

# Add x-axis and y-axis labels, and a title to  sp2
sp2 %>%
  layout(
    xaxis = list(title = ''),
    xaxis2 = list(title = 'Year'),
    yaxis = list(title = "Global Sales (M units)"), 
    yaxis2 = list(title = "Global Sales (M units)")
  )
# NOTE '' for blank title


### Scatterplot Matrices
data %>% 
  plot_ly() %>% 
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label = 'esdfsef', values = ~Alcohol)
    )
  )
# Neat interactionsd with lasso here - highlight specific points in all plots


###
# Create a SPLOM of NA_Sales, EU_Sales, and JP_Sales
vgsales2016 %>%
  plot_ly() %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label = 'N. America', values = ~NA_Sales),
      list(label = 'Europe', values = ~EU_Sales),
      list(label = 'Japan', values = ~JP_Sales)
    )
  )

# Color the SPLOM of NA_Sales, EU_Sales, and JP_Sales by nintendo
vgsales2016 %>%
  mutate(nintendo = ifelse(Publisher == "Nintendo", "Nintendo", "Other")) %>%
  plot_ly(color = ~nintendo, colors = c("orange", "black")) %>% 
  add_trace(
    type = 'splom',                                  
    dimensions = list(
      list(label = 'N. America', values = ~NA_Sales),
      list(label = 'Europe', values = ~EU_Sales),
      list(label = 'Japan', values = ~JP_Sales)      
    )
  )
# This color variable in plotly call is confusing

# Delete the plots in the upper half of splom
splom %>%
  style(showupperhalf = FALSE)

# Delete the diagonal plots in splom
splom %>%
  style(diagonal = list(visible = FALSE))

# Delete the plots in the lower half of splom
splom %>%
  style(showlowerhalf = FALSE)


### Binned Scatterplots
vgsales %>%
  plot_ly(x = ~Critic_Score, y = ~User_Score) %>%
  add_histogram2d(nbinsx = 50, nbinsy = 50)



# Chapter 4: Maps ---------------------------------------------------------


# Create a scatterplot of turnout2018 against turnout2014
turnout %>%
  plot_ly(x = ~turnout2014, y = ~turnout2018) %>%
  add_markers() %>%
  layout(xaxis = list(title = '2014 voter turnout'),
         yaxis = list(title = '2018 voter turnout'))

# Add the line y = x to the scatterplot
p %>%
  add_lines(x = c(.25, .6), y = c(.25, .6)) %>%
  layout(showlegend = FALSE)
# NOTE: I do not like how these points are oriented. x1 x2 and y1 y2. No sense

# Create a dotplot of voter turnout in 2018 by state ordered by turnout
turnout %>%
  slice_max(turnout2018, n = 15) %>%
  plot_ly(x = ~turnout2018, y = ~fct_reorder(state, turnout2018)) %>%
  add_markers() %>%
  layout(xaxis = list(title = 'Eligible voter turnout'), 
         yaxis = list(title = 'State', type = 'category'))
# Note that I hate how we convert to category here

# Create a histogram of receipts for the senate races
fundraising %>%
  filter(office == 'S') %>%
  plot_ly(x = ~receipts) %>%
  add_histogram() %>%
  layout(xaxis = list(title = 'Total contributions received'),
         title = 'Fundraising for 2018 Senate races')

# Create a dotplot of the top 15 Senate campaigns
fundraising %>%
  filter(office == "S") %>%
  slice_max(receipts, n = 15) %>%
  plot_ly(x = ~receipts, y = ~fct_reorder(state, receipts),
          color = ~fct_drop(party),
          hoverinfo = "text",
          text = ~paste("Candidate:", name, "<br>",
                        "Party:", party, "<br>",
                        "Receipts:", receipts, "<br>",
                        "Disbursements:", disbursement)) %>%
  add_markers(colors = c("blue", "red")) 


### Chloropleth Maps
turnout %>% 
  plot_geo(locationmode = 'USA-states') %>% 
  add_trace(
    z = ~turnout, # sets color values
    locations = ~state.abbr # matches cases to polygons
  ) %>% 
  layout(geo = list(scope = 'usa'))
# scope, projection, scale, center = list(lat = ~c.lat, long = ~c.lon)


###
# Create a choropleth map of the change in voter turnout from 2014 to 2018
turnout %>%
  mutate(change = turnout2018 - turnout2014) %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(
    z = ~change,
    locations = ~state.abbr) %>%
  layout(geo = list(scope = 'usa'))

# Create a choropleth map displaying the Senate results
senate_winners %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_trace(z = ~as.numeric(party), locations = ~state,
            colors = c("dodgerblue", "mediumseagreen", "tomato"),
            hoverinfo = "text",
            text = ~paste("Candidate:", name, "<br>",
                          "Party:", party, "<br>",
                          "% vote:", round(pct.vote, 1))
  ) %>%
  layout(geo = list(scope = 'usa')) %>% 
  hide_colorbar()

# Map President Trump's rallies in 2018
rallies2018 %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_markers(
    x = ~long, 
    y = ~lat, 
    size = ~no.speakers,  
    hoverinfo = "text", text = ~paste(city, state, sep = ",")
  ) %>%
  layout(title = '2018 Trump Rallies', 
         geo = list(scope = 'usa'))

# Customize the geo layout
g <- list(scope = 'usa', 
          showland = TRUE, landcolor = toRGB('gray90'),
          showlakes = TRUE, lakecolor = toRGB('white'),
          showsubunit = TRUE, subunitcolor = toRGB('white'))

# Apply the geo layout to the map
rallies2018 %>%
  plot_geo(locationmode = 'USA-states') %>%
  add_markers(
    x = ~long, y = ~lat, size = ~no.speakers, 
    hoverinfo = "text", text = ~paste(city, state, sep = ",")
  ) %>%
  layout(title = "2018 Trump Rallies", geo = g)
# Note that we just pass the list g straight in


### Polygons to Maps
# Define the layout settings to polish the axes
map_axes <- list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)

# Apply the layout to both axes
senate_map %>%
  group_by(group) %>%
  plot_ly(x = ~long, y = ~lat, color = ~party, split = ~region,
          colors = c("dodgerblue", "mediumseagreen", "tomato")) %>%
  add_polygons(line = list(width = 0.4, color = toRGB("gray60")), showlegend = FALSE) %>%
  layout(xaxis = map_axes, yaxis = map_axes)

# Join the fl_boundaries and fl_results data frames
senate_vote <- left_join(fl_boundaries, fl_results, by = c("subregion" = "CountyName"))

# Specify the axis settings to polish the map
map_axes <- list(
  title = "",
  showgrid = FALSE,
  zeroline = FALSE,
  showticklabels = FALSE)

# Create a polished county-level choropleth map of Pctvote
senate_vote %>%
  group_by(group) %>%
  plot_ly(x = ~long, y = ~lat, 
          color = ~Pctvote,
          split = ~subregion) %>%
  add_polygons(line = list(width = 0.4), showlegend = FALSE, colors = c("blue", "red")) %>%
  layout(xaxis = map_axes, yaxis = map_axes)

# This course kinda sucked
