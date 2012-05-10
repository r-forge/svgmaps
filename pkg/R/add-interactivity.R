### adding interactivity to a grob ############################################


##' Adds interactivity to a grob
##'
##' This is a generic function which takes a grob and a datasets and
##' garnishes the grob with the data. There is tooltip, highlight and link implemented now.
##' 
##' @title add-interactivity
##' @rdname add-interactivity
##' @param gr A grob object
##' @param data A data frame
##' @S3method add_interactivity zeroGrob
##' @S3method add_interactivity grob
##' @export
##' @return A garnished grob
##' @author chris
add_interactivity <- function (gr, data){
  ## is interactivity specified at all?
  inter <- intersect(c("tooltip", "highlight", "link"), names(data))
  if(length(inter) == 0) {
    return(gr)
  }
  
  UseMethod("add_interactivity", object = gr)
}

add_interactivity.zeroGrob <- function (gr, data) {
  return(gr)
}

##' Add interactivity to points grob
##'
##' Add interactivity to points grob
##' @param gr A points grob
##' @param data The data containing interactivity values
##' @return A garnished points grob
##' @S3method add_interactivity points
##' @author chris
add_interactivity.points <- function (gr, data){
  args <- list(x = gr, group = FALSE)
  for (interact in inter) {
    arg <- switch(interact,
                  "tooltip" = list(tooltip = data$tooltip),
                  "link" = list(link = data$link),
                  "highlight" = list(highlight = data$highlight)
                  )
    args <- c(args, arg)
  }
  do.call(gridSVG::garnishGrob, args)
}



add_interactivity.grob <- function (gr, data) {
    args <- list(x = gr, group = FALSE)
  for (interact in inter) {
    arg <- switch(interact,
                  "tooltip" = list(tooltip = data$tooltip[1]),
                  "link" = list(link = data$link[1]),
                  "highlight" = list(highlight = data$highlight[1])
                  )
    args <- c(args, arg)
  }
  do.call(gridSVG::garnishGrob, args)
}

## add_interactivity.polyline <- function (gr, data, ...) {
##   args <- list(x = gr, group = FALSE)
##   for (interact in inter) {
##     arg <- switch(interact,
##                   "tooltip" = list(tooltip = data$tooltip[1]),
##                   "link" = list(link = data$link[1]),
##                   "highlight" = list(highlight = data$highlight[1])
##                   )
##     args <- c(args, arg)
##   }
##   do.call(gridSVG::garnishGrob, args)
## }

## add_interactivity.gTree <- add_interactivity.polyline
## add_interactivity.rect <- add_interactivity.polyline


