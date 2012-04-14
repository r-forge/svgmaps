
geom_ipolygon <- function (mapping = NULL,
                           data = NULL,
                           stat = "identity",
                           position = "identity",
                           na.rm = FALSE, ...) {
  GeomPolygonI$new(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 na.rm = FALSE, ...)
}


GeomPolygonI <- proto(ggplot2:::GeomPolygon, {
  draw <- function(.,  data, scales, coordinates, ...){
    gs <- ggplot2:::GeomPolygon$draw(data, scales, coordinates, ...)
    gs2 <- garnishGrob(gs, tooltip = data$tooltip[1], onmouseover = "showTooltip(evt)", group = FALSE)
  }
  # default_aes <- function(.) aes(colour="grey20", size=2, shape = 16,  alpha = 1, fill = NA)
}
                    )
