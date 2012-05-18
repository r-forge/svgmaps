##' Interactive Tiles
##'
##' The tiles igeom is used to create interactive spatial tiles
##' @inheritParams ggplot2::geom_tile
##' @export
##' @author chris
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
    add_interactivity(gs, data)
  }
  def_iaes <- aes(tooltip = "", link = NA, view = 0)
  default_aes <- function(.) c(def_iaes, GeomTile$default_aes())
})
