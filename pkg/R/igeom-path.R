igeom_path <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, arrow = NULL, ...) {
  if (!is.null(data)) data <- as_svgmaps(data, ...)
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
  default_aes <- function(.) aes(colour="black", size=0.5, linetype=1, alpha = 1)
  guide_geom <- function(.) "path"
}
                    )