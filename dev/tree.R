# Tree Diagram



# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  collapsibleTree,
  shinipsum
)

# Load
raw <- read.csv('dev/framework.csv')
get_str(raw)

# Wrangle
levels <- c('Dimension', 'Index', 'Indicator')
dat <- raw %>% 
  setNames(c(levels)) %>% 
  mutate(tooltip = random_text(nwords = 25)) %>% 
  group_by(Dimension) %>%
  mutate(
    count = n(),
    count_ = count,
    count_x1.25 = count * 1.25,
    count_x1.5 = count * 1.5,
    count_x2 = count * 2,
    Nodes = 1
  ) %>%
  ungroup() %>% 
  mutate(
    color = c(
      'grey'
    )
  )
get_str(dat)



# Example -----------------------------------------------------------------


# input data must be a nested data frame:
head(warpbreaks)

# Represent this tree:
# p <- collapsibleTree( warpbreaks, c("wool", "tension", "breaks"))
# p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dendrogram_interactive.html"))



# SM Tree -----------------------------------------------------------------


sm <- collapsibleTreeSummary(
  dat, 
  c(levels),
  tooltip = TRUE,
  nodeSize = 'count_',
  # root = 'Sustainability Metrics',
  attribute = 'Nodes',
  fontSize = 14,
  linkLength = 400
  # inputId = ?
)
sm

# inputId to get info from the last selected node.
# use this to show information about it
# what the indicators are, where the data come from