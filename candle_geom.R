library(grid)
library(ggplot2)
library(readr)
library(dplyr)

candleGrob <- function(xc, yc, openc, closec, highc, lowc) {
  
}

candleGrobs <- function(x, y, open, close, high, low) {
  #x,y are the center of the candle rectangle
  #high,low define the top and bottom of the glyph
  #open/close can be in either order and define which
  #of two colors fills the rect
  #total_height <- high - low
  #rect_height <- max(open,close) - min(open,close)
  t <- ifelse( open > close, open, close )
  td <- high - t
  b <- ifelse( open > close, close, open )
  bd <- b - low
  h <- t - b
  f <- ifelse( open > close, "red", "green" )
  #TODO: update this to return a gList() of two
  # lineGrob() and the rectGrob()
  #TODO: do it the same way as "arrow_geom", i.e.
  # calling mapply() to generate the gList of 
  # gTrees generated by candleGrob.  best thing to
  # do is pass in x, y, t, td, bd, b, h and color
  # and let candleGrob generate a gTree with three
  # children - one for the upper line, one for the
  # lower line, and one for the rectangle
  gTree(
    children = gList(
      rectGrob(x, y, 0.005, h, gp = gpar(fill = f, col = 0))
    )
  )
}

candle_Drawpanel <- function(data, panel_scales, coord, na.rm = FALSE) {
  coords <- coord$transform(data, panel_scales)
#  print(head(coords))
  ggplot2:::ggname("geom_candlesticks", 
        candleGrobs(coords$x, coords$y, coords$open, coords$close, coords$high, coords$low))
}

geom_candlesticks <- function(mapping = NULL, data = NULL, stat = "identity", 
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = gCandles, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, ...)
  )
}

gCandles <- ggproto( "gCandles", Geom, 
                   draw_panel = candle_Drawpanel, 
                   non_missing_aes = c("x", "y", "open", "close", "high", "low"),
                   default_aes = aes(fill = "lightgreen", alpha = "0.5", size = 1),
                   icon = function(.) {}, 
                   desc_params = list(), 
                   seealso = list(), 
                   examples = function(.) {})


lhsif <- read_csv("LHSIF.csv") %>%
  mutate(y_med = Low + (High - Low)/2)

ggplot(lhsif, aes(x = Date, y = y_med, open = Open, high = High, close = `Adj Close`, low = Low)) +
  geom_candlesticks()
