##' Interactive Points
##'
##' The ipoint geom is used to add interactive points to a plot
##'
##' With this geom u can do the same stuff as with the original \code{geom_point}
##' from ggplot2 plus some additional mapping for interactivity.
##' See the man page of \code{geom_point} for more details on the static mapping
##' values (as colour, shape, size, ...)
##'
##' @section Aesthetics
##'
##' \code{geom_ipoint} understands the following aesthetics:
##'
##' \itemize{
##'   \item \code{x}; horizontal position
##'   \item \code{y}: vertical position
##'   \item \code{shape}: point shape
##'   \item \code{colour}: point colour
##'   \item \code{fill}: fill colour, only affects solid points
##'   \item \code{size}: size
##'   \item \code{alpha}: alpha transparency modifies colour
##'   \item \code{tooltip}: onmouseover tooltip (only in SVG)
##'   \item \code{highlight}: onclick highlighting (only in SVG)
##'   \item \code{link}: hyperlink to website (only in SVG)
##' @title 
##' @param mapping 
##' @param data 
##' @param stat 
##' @param position 
##' @param na.rm 
##' @param ... 
##' @return 
##' @author chris
igeom_point <- function (mapping = NULL,
                         data = NULL,
                         stat = "identity",
                         position = "identity",
                         na.rm = FALSE, ...) {
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
    gs <- ggplot2:::GeomPoint$draw(data, scales, coordinates, ...)
    args <- list(x = gs, group = FALSE)
    if ("tooltip" %in% names(data)){
      args <- c(args, list(tooltip = data$tooltip, onmouseover = rep("showTooltip(evt)", nrow(data))))
    }
    if ("highlight" %in% names(data)){
      args <- c(args, list(highlight = TRUE))
    }
    browser()
    do.call(garnishGrob, args)
  }
  default_aes <- function(.) aes(colour="grey20", size=2, shape = 16,  alpha = 1, fill = NA)
})

