igeom_tile <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", ...) {
  if (!is.null(data)) data <- as_svgmap(data)
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
    default_aes <- function(.) c(aes(tooltip = ""), GeomTile$default_aes())
})
