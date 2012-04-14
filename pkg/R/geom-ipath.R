geom_ipath <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
lineend = "butt", linejoin = "round", linemitre = 1, na.rm = FALSE, arrow = NULL, ...) {
  GeomPathI$new(mapping = mapping, data = data, stat = stat, position = position,
  lineend = lineend, linejoin = linejoin, linemitre = linemitre, na.rm = na.rm, arrow = arrow, ...)
}

GeomPathI <- proto(ggplot2:::GeomPolygon, {
  draw <- function(.,  data, scales, coordinates, ...){
    gs <- ggplot2:::GeomPath$draw(data, scales, coordinates, ...)
    gs2 <- garnishGrob(gs, tooltip = data$tooltip[1], onmouseover = "showTooltip(evt)", group = TRUE)
    gs2
  }
  # default_aes <- function(.) aes(colour="grey20", size=2, shape = 16,  alpha = 1, fill = NA)
}
                    )
