
igeom_polygon <- function (mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE, ...) {
  IGeomPolygon$new(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 na.rm = FALSE, ...)
}


IGeomPolygon <- proto(ggplot2:::GeomPolygon, {
  objname <- "ipolygon"
  draw <- function(.,  data, scales, coordinates, ...){
    ## make a subset
    # data <- subset(data, subset = data$geom == "point")
    gs <- ggplot2:::GeomPolygon$draw(data, scales, coordinates, ...)
    gs2 <- garnishGrob(gs, tooltip = data$tooltip[1], onmouseover = "showTooltip(evt)", group = FALSE)
  }
  # default_aes <- function(.) aes(colour="grey20", size=2, shape = 16,  alpha = 1, fill = NA)
}
                    )
