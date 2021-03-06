library(ggplot2)
library(grid)

lineArrowGrob <- function(xc, yc, size, angle, colour = "darkgreen", linetype=1) {
  sinA <- sin(angle)
  cosA <- cos(angle)
  l <- 0.5*size
  x = c(xc - l*cosA, xc + l*cosA)
  y = c(yc - l*sinA, yc + l*sinA)
  gl <- gList(
    linesGrob(
      x = x, y = y,
      default.units =  "native",
      gp = gpar(col = colour, lwd = 1*ggplot2:::.pt, lty=linetype, arrow = arrow(45, type = "closed"), lineend = "butt")
    )
  )
  gl
}

myArrowsGrob <- function(x, y, size, angle, colour) {
  sizeMax <- max(size)
  sizeMin <- min(size)
  scaledSize <- 0.005 + 0.1 * (size - sizeMin) / (sizeMax - sizeMin)  
  listOfGrobs <- mapply(function(xc, yc, size, angle, colour) {
    lineArrowGrob(xc, yc, size, angle, colour)
  }, x, y, scaledSize, angle, colour)
  class(listOfGrobs) <- "gList"
  gt <- gTree(name = "arrowz", children = listOfGrobs, gp = NULL)
}

arrows_DrawPanel <- function(data, panel_scales, coord, na.rm = FALSE) {
  coords <- coord$transform(data, panel_scales)
  ggplot2:::ggname("geom_arrows", 
        myArrowsGrob(coords$x, coords$y, coords$size, coords$angle, coords$colour)
  )
}

geom_arrows <- function(mapping = NULL, data = NULL, stat = "identity", 
              position = "identity", na.rm = FALSE, show.legend = NA, 
              inherit.aes = TRUE, ...) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = gArrows, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, ...)
  )
}

gArrows <- ggproto( "gArrows", Geom, 
                    draw_panel = arrows_DrawPanel, 
                    non_missing_aes = c("x", "y", "size", "angle", "colour"),
                    default_aes = aes(),
                    icon = function(.) {}, 
                    desc_params = list(), 
                    seealso = list(), 
                    examples = function(.) {})

set.seed(123)

dc <- mtcars[,c("wt", "mpg", "hp", "cyl")]
angle <- runif(1:32) * 360
dc <- cbind(dc, angle)

ggplot(dc, aes(wt, mpg, size=hp, col = cyl, angle = angle)) + 
  geom_arrows()
