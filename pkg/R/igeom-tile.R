igeom_tile <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) {
  IGeomTile$new(mapping = mapping, data = data, stat = stat, position = position, ...)
}

IGeomTile <- proto(ggplot2:::GeomTile, {
  objname <- "itile"
  draw_groups<- function(., data, scales, coordinates, ...){
    ## make a subset
    # data <- subset(data, subset = data$geom == "point")
    gs <- GeomTile$draw_groups(data,scales, coordinates, ...)
    garnishGrob(gs, tooltip = data$tooltip, onmouseover = rep("showTooltip(evt)", nrow(data)), group = FALSE)
  }
})


x <- c(rep(0.3, 3), rep(0.5, 3), rep(0.7, 3))
y <- c(rep(c(0.3,0.5, 0.7),3))

df <- data.frame(x, y, z = 1:9)
ggplot() + geom_itile(aes(x = x, y = y, fill = z, tooltip = z), df)

grid.script(filename="../demo/tooltip2.js")
gridToSVG("test.svg")
