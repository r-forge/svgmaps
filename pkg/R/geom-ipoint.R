
geom_ipoint <- function (mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE, ...) {
  GeomPointI$new(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 na.rm = FALSE, ...)
}

GeomPointI <- proto(ggplot2:::GeomPoint, {
  draw <- function(.,  data, scales, coordinates, ...){
    gs <- ggplot2:::GeomPoint$draw(data, scales, coordinates, ...)
    gs2 <- garnishGrob(gs, tooltip = data$tooltip, onmouseover = rep("showTooltip(evt)", nrow(data)), group = FALSE)
    gs2
  }
  default_aes <- function(.) aes(colour="grey20", size=2, shape = 16,  alpha = 1, fill = NA)
}
                    )

