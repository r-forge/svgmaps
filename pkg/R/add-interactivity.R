### adding interactivity to a grob ############################################

## for link:
# cat(paste("onclick=\"window.open('", link, "', '_blank', ''); return false;\"", sep = ""))

##' Adds interactivity to a grob
##'
##' This is a generic function which takes a grob and a datasets and
##' garnishes the grob with the data. There is tooltip, highlight and link implemented now.
##' 
##' @title add-interactivity
##' @param gr A grob object
##' @param data A data frame
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



add_interactivity.points <- function (gr, data){
  args <- list(x = gr, group = FALSE)
  for (interact in inter) {
    arg <- switch(interact,
                  "tooltip" = list(tooltip = data$tooltip, 
                    onmouseover = rep("showTooltip(evt)", nrow(data)),
                    onmouseout = rep("hideTooltip(evt)", nrow(data))), 
                  "link" = list(link = data$link),
                  "highlight" = list(onmouseover = "highlight(evt)", onmouseout = "downlight(evt)")  ## to be implemented
                  )
    args <- c(args, arg)
  }
  do.call(gridSVG::garnishGrob, args)
}

add_interactivity.polyline <- function (gr, data, ...) {
  args <- list(x = gr, group = TRUE)
  for (interact in inter) {
    arg <- switch(interact,
                  "tooltip" = list(tooltip = data$tooltip[1],
                    onmouseover = "showTooltip(evt)",
                    onmouseout = "hideTooltip(evt)"),
                  "link" = list(link = data$link[1]),
                  "highlight" = list(onmouseover = "highlight(evt)", onmouseout = "downlight(evt)")
                  )
    args <- c(args, arg)
  }
  do.call(gridSVG::garnishGrob, args)
}


add_interactivity.gTree <- add_interactivity.polyline

add_interactivity.rect <- function (gr, data) {
  
}

add_interactivity.zeroGrob <- function(gr, data) {
  return(gr)
}
