library(ggplot2)
library(grid)

################################################################################
# todo: change this to generate a net flow plot.  this needs the following 
# aesthetics: x, y, y_in, y_out
################################################################################

scale_Values <- function(values, vFloor, vCeil) {
  vMin = min(values)
  vMax = max(values)
  vFloor + vCeil * (values - vMin) / (vMax - vMin)
}

boxGrob <- function(x, y, width, height, fill, alpha) {
  scaledWidth <- scale_Values(width, 0.005, 0.1)
  scaledHeight <- scale_Values(height, 0.005, 0.1)
  gp = gpar(fill = fill, alpha = alpha)
  #this is really a set of lineGrob()s
  #
  rectGrob(x, y, scaledWidth, scaledHeight, gp = gp)
}

crosses_DrawPanel <- function(data, panel_scales, coord, na.rm = FALSE) {
  coords <- coord$transform(data, panel_scales)
  ggplot2:::ggname("geom_flow", 
                   boxGrob(coords$x, coords$y, coords$width, coords$height, coords$fill, coords$alpha))
}

#define the body of the geom
geom_crosses <- function(mapping = NULL, data = NULL, stat = "identity", 
                       position = "identity", na.rm = FALSE, show.legend = NA, 
                       inherit.aes = TRUE, ...) {
  layer(
    data = data, 
    mapping = mapping, 
    stat = stat, 
    geom = gCrosses, 
    position = position, 
    show.legend = show.legend, 
    inherit.aes = inherit.aes, 
    params = list(na.rm = na.rm, ...)
  )
}

gCrosses <- ggproto( "gCrosses", Geom, 
                   draw_panel = crosses_DrawPanel, 
                   non_missing_aes = c("x", "y", "width", "height"),
                   default_aes = aes(fill = "lightgreen", alpha = "0.5", size = 1),
                   icon = function(.) {}, 
                   desc_params = list(), 
                   seealso = list(), 
                   examples = function(.) {})

dc <- mtcars[,c("wt", "mpg", "drat", "qsec", "carb", "cyl")]

ggplot(dc, aes(x=wt, y=mpg, width=drat, height=qsec)) +
  geom_crosses(aes(alpha = carb, fill = cyl))
