##' Interactive Polygons
##'
##' The polygon igeom is used to create interactive spatial points.
##'
##' For further documentation see ?ggplot2::geom_point
##' @inheritParams ggplot2::geom_polygon
##' @export
##' @author chris
igeom_polygon <- function (mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE, ...) {
  if (!is.null(data)) data <- as_svgmap(data)
  IGeomPolygon$new(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 na.rm = FALSE, ...)
}


IGeomPolygon <- proto(ggplot2:::GeomPolygon, {
  objname <- "ipolygon"
  draw <- function(.,  data, scales, coordinates, ...){
    if (unique(data$geom) != "polygon") return(zeroGrob())
    gr <- ggplot2:::GeomPolygon$draw(data, scales, coordinates, ...)
    add_interactivity(gr, data)
  }
  default_aes <- function(.) c(aes(tooltip = ""), GeomPolygon$default_aes())
}
                    )
