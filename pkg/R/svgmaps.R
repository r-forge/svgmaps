##' svgmaps
##'
##' @name svgmaps
##' @docType package
##' @aliases svgmaps package-svgmaps
##' @import proto reshape2 grid gridSVG plyr
NULL






##' ##' ggplot() like function for interactive maps
##'
##' This function can be used similiar to ggplot(). It is limited to input objects which can be handled by
##' as_svgmap. Currently these are objects with class SpatialXXXDataFrame, osmar and svgmap_df.
##' @title svgmap
##' @param data 
##' @param ...
##' @return An object of with class svgmap and ggplot. Can be used like a normal ggplot2 object, but
##' also with the functions view_svgmap and save_svgmap.
##' @export
##' @author chris
svgmap <- function (data = NULL, ...) {
  if (!is.null(data)) {
    data <- as_svgmap(data, ...)
  }
  p <- ggplot(data, mapping = aes_string(x = "lon", y = "lat", geom = "geom", group = "element_id")) + coord_equal()
  # set interactive options
  p$ioptions <- list(hcolour = "yellow")
  class(p) <- c("svgmap", class(p))
  p
}





##' Generic function which transforms spatial objects into the svgmap_df data frame format.
##'
##' Works like fortify
##' @title as_svgmap
##' @param object Spatial objects; Currently osmar and SpatialXXXDataFrame objects
##' @param ... Not yet in use
##' @return A svgmap_df data frame
##' @export
##' @S3method as_svgmap default
##' @S3method as_svgmap NULL
##' @S3method as_svgmap data.frame
##' @author chris
as_svgmap <- function(object, ...){
  UseMethod("as_svgmap")
}

as_svgmap.default <- function (object, ...) {
  stop(paste("as_svgmap does not support objects of class", class(object), "\n For supported classes type: showMethods(as_svgmap)"))
}

as_svgmap.NULL <- function (object, ...) {
  return(NULL)
}

as_svgmap.data.frame <- function (object, ...){
  if (!is_svgmap(object)) stop("The data.frame is not in the svgmap format")
  return(object)
}


##' Check svgmap data frame
##'
##' Checks if the object is in valid svgmap data frame format
##' This function takes a data frame and checks if it is a valid svgmap_df object. This means it has to be a data frame with columns named lon, lat, element_id, point_id, geom, order and an arbitrary number of additional variables. In addition it has to have the class svgmap_df
##' @param df A data frame
##' @return logical; TRUE if object is a valid svgmap_df
##' @export
is_svgmap <- function (df) {
  stopifnot(inherits(df, "data.frame"))
  namez <- c("lon", "lat", "element_id", "point_id", "geom", "order")
  if(inherits(df, "svgmap_df") & all(namez %in% names(df))) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

## TODO: suppress the GUI-Device
##' Save svgmap
##' 
##' Saves a svgmap object as SVG.
##'
##' This function adds the necessary scripts for interactivity and converts the plot into an SVG file.
##' @param object svgmap object
##' @param filename the filename of the SVG file
##' @export
##' @author chris
save_svgmap <- function (object, filename = "RPlot.svg") {
  gr <- ggplotGrob(object)
  ## insert here: function to add variable from ioptions to javascript
  #--------
  ## better solution here: define scripts; make gList of all scriptGrobs; use setChildren(gr, scriptGrobList)
  js_dir <- system.file("javascript", package = "svgmaps")
  script <- "http://ajax.googleapis.com/ajax/libs/jquery/1.3/jquery.min.js"
  igr <- grid::addGrob(gr, gridSVG::scriptGrob(filename = script, inline = TRUE))
  script <- file.path(js_dir, "add-events.js")
  igr <- grid::addGrob(igr, gridSVG::scriptGrob(filename = script, inline = TRUE))
  script <- file.path(js_dir, "tooltip.js")
  igr <- grid::addGrob(igr, gridSVG::scriptGrob(filename = script, inline = TRUE))
  script <- file.path(js_dir, "highlight.js")
  igr <- grid::addGrob(igr, gridSVG::scriptGrob(filename = script, inline = TRUE))
  

  ## Open a new SVG-Device
  svgdev <- gridSVG:::openSVGDev(filename, width=par("din")[1], height=par("din")[2])
  ## Translate grid object, write
  gridSVG:::gridToDev(igr, svgdev)
  ## Close Device
  gridSVG:::devClose(svgdev)
}



view_svgmap <- function (p) {
  tmpdir = tempdir()
  tmpfile <- tempfile(pattern = "plot", tmpdir = tmpdir, fileext = ".svg")
  save_svgmap(p, tmpfile)
  ## Open Browser
  browseURL(tmpfile)
}



iopts <- function (...) 
{
    structure(list(...), class = "ioptions")
}


"+.svgmap" <- function(p, object) {
  if (inherits(object, "ioptions")){
    object$labels <- defaults(object$labels, p$ioptions$labels)
    p$ioptions <- defaults(object, p$ioptions)
    p
  } else {
    NextMethod("+")
  }
}