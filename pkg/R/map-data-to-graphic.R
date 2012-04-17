
##' Checks if Mapping is valid for Element
##'
##' This function returns TRUE, if the mapping method is
##' valid for the element. What is allowed is mainly dictated by the
##' aesthetics options for the fitting geom from ggplot2
##' @title check Mapping
##' @param element Graphical Element, e.g. lines, points, ...
##' @param mapping Graphical Attributes, e.g. colour, toolTip, ..
##' @return TRUE if mapping is valid for element, else FALSE
##' @author chris
checkMapping <- function(element, mapping){
  valid_all<- c("colour", "toolTip", "alpha")
  valid_special <- switch(element,
                       line=c("shape", "size"),
                       point=c("shape", "size"),
                       polygon=c("linetype", "size"),
                       grid=c("fill", "linetype", "size")
                       )
  valid_mapp <- c(valid_all, valid_special)
  mapping %in% valid_mapp
}



get_bbox <- function(object){
  if ("osmar" %in% class(object)) cbind(lat = range(object$nodes$attrs$lat), lon = range(object$nodes$attrs$lon)) else
  sp::bbox(object)
}
#get_bbox(home.sp$points)
