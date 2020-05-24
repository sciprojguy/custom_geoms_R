library(ggplot2)
library(grid)

##############################################################################
#circles geom
##############################################################################
myGeom_DrawPanel <- function(data, panel_scales, coord, na.rm = FALSE) {
  coords <- coord$transform(data, panel_scales)
  #note: ggname assigns a namespace entry to myGeomGrob and (i guess) returns the object
  ggplot2:::ggname("geom_myGeom", myGeomGrob(coords$x, coords$y, coords$size, coords$fill))
}

myGeomGrob <- function(x, y, size, fill) {
  circleGrob(x = x, y = y, r = size, gp = gpar(col = fill), default.units = "npc")
}

geom_myGeom <- function(mapping = NULL, data = NULL, stat = "identity", position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
        data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = myGeom, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...)
  )
}

myGeom <- ggproto( 
            "myGeom", 
            Geom, 
            draw_panel = myGeom_DrawPanel, 
            non_missing_aes = c("x", "y"),
            default_aes = aes(size = 0.01, fill = "red"),
            icon = function(.) {}, 
            desc_params = list(), 
            seealso = list(), 
            examples = function(.) {})

ggplot( mtcars, aes(wt, mpg, size = qsec) ) + 
  geom_myGeom()

