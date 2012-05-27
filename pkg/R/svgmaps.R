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
##' @param data The data, can currently be a SpatialXXXDataFrame or an osmar object
##' @param ... Not in use
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
  p$ioptions <- list(hcolour = "yellow", hsize = 1)
  class(p) <- c("svgmap", class(p))
  p
}





##' Generic function which transforms spatial objects into the svgmap_df data frame format.
##'
##' Works like fortify
##' This function takes spatial data (currently sp and osmar) and transforms it into a suitable way for
##' the svgmaps package, which is a special data frame. For SpatialLinesDataFrame and SpatialPolygonsDataFrame
##' it is assumed, that the ids are the rownames.
##' @title as_svgmap
##' @param object Spatial objects; Currently osmar and SpatialXXXDataFrame objects
##' @param ... Not yet in use
##' @return A svgmap_df data frame
##' @S3method as_svgmap default
##' @S3method as_svgmap NULL
##' @S3method as_svgmap data.frame
##' @S3method as_svgmap map
##' @export
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
  namez <- c("lon", "lat", "element_id", "point_id", "geom", "order")
  if (is_svgmap(object)) {
    return(object)
  } else if (all(namez %in% names(object))) {
    class(object) <- c("svgmap_df", class(object))
    return(object)
  } else {
   stop("Not in svgmaps format")
  }
}


as_svgmap.map <- function (object, ...) {
  df <- fortify(object)
  df <- rename(df, c(long = "lon", region =  "element_id"))
  df$point_id <- rownames(df)
  df$geom <- "polygon"
  class(df) <- c("svgmap_df", class(df))
  df
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


##' Add javascript files to the plot
##'
##' add_javascript
##' This function adds scriptGrobs to a given grob.
##' The javascript files to be added are those which are necessary for adding interactivity to a plot
##' @param grob A garnished Grob
##' @return Garnished Grob with additional scriptGrob children
add_javascript <- function (grob) {
  js_dir <- system.file("javascript", package = "svgmaps")
  
  scripts <- c("jquery-1.7.2.min.js",
               # "jquery.qtip.js",
               "link.js",
               "tooltip.js",
               "highlight.js",
               "view.js"
               )
  
  add_script <- function (script_str) {
    script <- file.path(js_dir, script_str)
    gridSVG::scriptGrob(filename = script, inline = TRUE)
  }
  script_list <- llply(.data = scripts, .fun = function (x) { add_script(x)})
  script_grobs <- do.call(gList, script_list)
  igr <- setChildren(grob, gList(grob$children, script_grobs))
  igr
}


opts_to_js <- function(iopts) {
  iopts$hcolour <- paste("rgb(", paste(col2rgb(iopts$hcolour), collapse = ",", sep = ""), ")", sep = "")
  paste("var ", names(iopts), " = \'", iopts, "\'", collapse = "\n", sep = "")
}



add_javascript_vars <- function (grob, iopts) {
  script <- opts_to_js(iopts)
  script_grob <- scriptGrob(script = script, inline = TRUE, name = "vars")
  igrob <- addGrob(grob, script_grob)
}

## TODO: suppress the GUI-Device
##' Save svgmap
##' 
##' Saves a svgmap object as SVG.
##'
##' This function adds the necessary scripts for interactivity and converts the plot into an SVG file.
##' @param object  a svgmap object
##' @param filename the filename of the SVG file
##' @export
##' @author chris
save_svgmap <- function (object, filename = "RPlot.svg") {
  iopts <- object$ioptions
  gr <- ggplotGrob(object)
  ## function to add variable from ioptions to javascript
  igr <- add_javascript_vars(gr, iopts)
  igr <- add_javascript(igr)
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


##' Set interactive properties
##'
##' Set interactive options.
##' Like the options in ggplot2, this can be used to control the plot appearance.
##' This controls the interactive porperties. Currently only hcolour is implemented.
##' Hcolour defines the colour in which the elements are highlighted. 
##' @title iopts
##' @param ... The named options
##' @return An ioptions object
##' @export
##' @author chris
iopts <- function (...) 
{
    structure(list(...), class = "ioptions")
}

##' Modify a plot by adding new layers
##'
##' Same as in ggplot2 but for svgmap()
##' @title "+.svgmap"
##' @param p plot object
##' @param object A object which shall be added to the plot
##' @return A svgmap object
##' @export
##' @method + svgmap
##' @S3method "+" svgmap
##' @rdname svgmap-add
##' @author chris
"+.svgmap" <- function(p, object) {
  if (inherits(object, "ioptions")){
    object$labels <- defaults(object$labels, p$ioptions$labels)
    p$ioptions <- defaults(object, p$ioptions)
    p
  } else {
    NextMethod("+")
  }
}

