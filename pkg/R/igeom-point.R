igeom_point <- function (mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE, ...) {
  data <- as_svgmaps(data)
  IGeomPoint$new(mapping = mapping,
                 data = data,
                 stat = stat,
                 position = position,
                 na.rm = FALSE, ...)
}

IGeomPoint <- proto(ggplot2:::GeomPoint, {
  objname <- "ipoint"
  draw <- function(.,  data, scales, coordinates, ...){
    ## make a subset
    data <- subset(data, subset = data$geom == "point")
    ## missings have to be removed before draw so
    ## that draw and add_interactivity work on same data
    data <- remove_missing(data, na.rm = FALSE, 
      c("x", "y", "size", "shape"), name = "geom_point")
    gs <- ggplot2:::GeomPoint$draw(data, scales, coordinates, ...)
    add_interactivity(gs, data)
  }
  default_aes <- function(.) c(aes(tooltip = ""), GeomPoint$default_aes())
})

