##' Interactive Paths
##'
##' The path igeom is used to create interactive spatial paths
##' @inheritParams ggplot2::geom_path
##' @export
##' @author chris
igeom_path <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, arrow = NULL, ...) {
  if (!is.null(data)) data <- as_svgmap(data)
  IGeomPath$new(mapping = mapping, data = data, stat = stat, position = position,
  lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, arrow = arrow, ...)
}

IGeomPath <- proto(ggplot2:::GeomPolygon, {
  objname <- "ipath"
  draw <- function(.,  data, scales, coordinates, ...){
    if(unique(data$geom) != "path") return(ggplot2:::zeroGrob())
    gs <- ggplot2:::GeomPath$draw(data, scales, coordinates, ...)
    add_interactivity(gs, data)
  }
  default_aes <- function(.) c(aes(tooltip = "", show = "0"), GeomPath$default_aes())
  guide_geom <- function(.) "path"
}
                    )
