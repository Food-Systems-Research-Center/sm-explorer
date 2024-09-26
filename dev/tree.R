# Tree Diagram



# Housekeeping ------------------------------------------------------------

pacman::p_load(
  dplyr,
  collapsibleTree,
  shinipsum,
  viridisLite
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
  ungroup()
get_str(dat)

vir <- viridis(5)
colors = c(
  'grey',
  vir[1],
  vir[2],
  vir[3],
  vir[4],
  vir[5],
  rep(vir[1], 10),
  rep(vir[2], 9),
  rep(vir[3], 6),
  rep(vir[4], 6),
  rep(vir[5], 5),
  rep(vir[1], 27),
  rep(vir[2], 42),
  rep(vir[3], 21),
  rep(vir[4], 21),
  rep(vir[5], 24)
)


# Example -----------------------------------------------------------------


# input data must be a nested data frame:
# head(warpbreaks)

# Represent this tree:
# p <- collapsibleTree( warpbreaks, c("wool", "tension", "breaks"))
# p

# save the widget
# library(htmlwidgets)
# saveWidget(p, file=paste0( getwd(), "/HtmlWidget/dendrogram_interactive.html"))



# SM Tree -----------------------------------------------------------------


sm <- collapsibleTree(
  dat, 
  levels,
  tooltip = TRUE,
  tooltipHTML = TRUE,
  nodeSize = 'count_',
  root = 'Sustainability Metrics',
  attribute = 'Nodes',
  fontSize = 14,
  linkLength = 400,
  fill = colors
  # inputId = ?
)
sm

# inputId to get info from the last selected node.
# use this to show information about it
# what the indicators are, where the data come from



# Network Example ---------------------------------------------------------


org <- data.frame(
  Manager = c(
    NA, "Ana", "Ana", "Bill", "Bill", "Bill", "Claudette", "Claudette", "Danny",
    "Fred", "Fred", "Grace", "Larry", "Larry", "Nicholas", "Nicholas"
  ),
  Employee = c(
    "Ana", "Bill", "Larry", "Claudette", "Danny", "Erika", "Fred", "Grace",
    "Henri", "Ida", "Joaquin", "Kate", "Mindy", "Nicholas", "Odette", "Peter"
  ),
  Title = c(
    "President", "VP Operations", "VP Finance", "Director", "Director", "Scientist",
    "Manager", "Manager", "Jr Scientist", "Operator", "Operator", "Associate",
    "Analyst", "Director", "Accountant", "Accountant"
  )
)

collapsibleTree(org, c("Manager", "Employee"), collapsed = FALSE)
collapsibleTreeNetwork(org, attribute = "Title", collapsed = FALSE)

###
org$Color <- org$Title
levels(org$Color) <- colorspace::rainbow_hcl(11)
collapsibleTreeNetwork(
  org,
  attribute = "Title",
  fill = "Color",
  nodeSize = "leafCount",
  collapsed = FALSE
)

###
org$tooltip <- paste0(
  org$Employee,
  "<br>Title: ",
  org$Title,
  "<br><img src='https://source.unsplash.com/collection/385548/150x100'>"
)

collapsibleTreeNetwork(
  org,
  attribute = "Title",
  fill = "Color",
  nodeSize = "leafCount",
  tooltipHtml = "tooltip",
  collapsed = FALSE
)
