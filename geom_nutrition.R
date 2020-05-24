library(ggplot2)
library(grid)

#this generates a composite geom
triangleGrob <- function(xc, yc, size, angle, fill = "darkgreen") {
  sinA = sin(angle)
  cosA = cos(angle)
  dx = c( -size/2, 0, size/2)
  dy = c( -size/2, size/2, -size/2)
  gl <- gList(
    polygonGrob(x = xc + (dx*cosA - dy*sinA), y = yc + (dx*sinA + dy*cosA), 
                gp = gpar(fill = fill))
  )
  gl
}

trianglePointsGrob <- function(x, y, size, angle, fill) {
  sizeMax <- max(size)
  sizeMin <- min(size)
  scaledSize <- 0.005 + 0.1 * (size - sizeMin) / (sizeMax - sizeMin)  
  listOfGrobs <- mapply(function(xc, yc, size, angle, fill) {
    triangleGrob(xc, yc, size, angle, fill)
  }, x, y, scaledSize, angle, fill)
  class(listOfGrobs) <- "gList"
  gt <- gTree(name = "triangles", children = listOfGrobs, gp = NULL)
}

triangles_DrawPanel <- function(data, panel_scales, coord, na.rm = FALSE) {
  coords <- coord$transform(data, panel_scales)
  ggplot2:::ggname("geom_arrows", 
                   trianglePointsGrob(coords$x, coords$y, coords$size, coords$angle, coords$fill)
  )
}

gTriangles <- ggproto( "gTriangles", Geom, 
                       draw_panel = triangles_DrawPanel, 
                       non_missing_aes = c("x", "y", "size", "angle", "fill"),
                       default_aes = aes(),
                       icon = function(.) {}, 
                       desc_params = list(), 
                       seealso = list(), 
                       examples = function(.) {})

geom_triangles <- function(mapping = NULL, data = NULL, stat = "identity", 
                           position = "identity", na.rm = FALSE, show.legend = NA, 
                           inherit.aes = TRUE, ...) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = gTriangles, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, ...)
  )
}

set.seed(123)

dc <- mtcars[,c("wt", "mpg", "hp", "cyl")]
angle <- runif(1:32) * 2.0*pi
dc <- cbind(dc, angle)

ggplot(dc, aes(wt, mpg, size=hp, fill = cyl, angle = angle)) + 
  geom_triangles() +
  scale_y_continuous(limits = c(0.0, 55))
