pacman::p_load(
  ggplot2,
  plotly,
  reshape,
  Hmisc,
  GGally
)
x <- Seatbelts
y <- as.matrix(x)
rt <- rcorr(y)
mtlr <- melt(rt$r)
mtlp <- melt(rt$P) 
p.value <- mtlp$value
gx <- ggplot(mtlr, aes(X1, X2, fill = value, label=p.value)) + geom_tile() + 
  scale_fill_gradient(low = "cyan",  high = "red")
ggplotly(gx)

# make ggcorrplot with seatbelts data set
ggcorrplot::ggcorrplot(rt$r, p.mat = rt$P, type = "lower", lab = TRUE)

ggcorr(y, layout.exp = 1.5, label = TRUE) %>% 
  ggplotly()



