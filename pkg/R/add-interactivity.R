### adding interactivity to a grob ############################################

## for link:
# cat(paste("onclick=\"window.open('", link, "', '_blank', ''); return false;\"", sep = ""))


add_interactivity <- function (gr, data){
  ## is interactivity specified at all?
  inter <- intersect(c("tooltip", "highlight", "link"), names(data))
  if(length(inter) == 0) {
    return(gr)
  }
  
  UseMethod("add_interactivity", object = gr)
}

data <- data.frame(tooltip = 1:10, link =10:19)


add_interactivity.points <- function (gr, data){
  args <- list(x = gr, group = FALSE)
  for (interact in inter) {
    arg <- switch(interact,
                  "tooltip" = list(tooltip = data$tooltip, 
                                   onmouseover = rep("showTooltip(evt)", nrow(data)),
                                   onmouseout = rep("hideTooltip(evt)", nrow(data))), 
                  "link" = list(onclick = paste("window.open('", data$link, "', '_blank', ''); return false;", sep = "")),
                  "highlight" = list(onmouseover = "highlight(evt)", onmouseout = "downlight(evt)")  ## to be implemented
                  )
    args <- c(args, arg)
  }
  do.call(garnishGrob, args)
}

add_interactivity.polyline <- function (gr, data, ...) {
  args <- list(x = gr, group = TRUE)
  for (interact in inter) {
    arg <- switch(interact,
                  "tooltip" = list(tooltip = data$tooltip[1],
                    onmouseover = "showTooltip(evt)",
                    onmouseout = "hideTooltip(evt)"),
                  "link" = list(onclick = paste("window.open('", data$link[1], "', '_blank', ''); return false;", sep = "")),
                  "highlight" = list(onmouseover = "highlight(evt)", onmouseout = "downlight(evt)")
                  )
    args <- c(args, arg)
  }
  do.call(garnishGrob, args)
}

add_interactivity.polygon <- function (gr, data, ...) {
  add_interactivity.polyline(gr, data, ...)
}


add_interactivity.gTree <- function (gr, data, ...) {
  args <- list(x = gr, group = FALSE)
  for (interact in inter) {
    arg <- switch(interact,
                  "tooltip" = list(tooltip = data$tooltip[1],
                    onmouseover = "showTooltip(evt)",
                    onmouseout = "hideTooltip(evt)"),
                  "link" = list(onclick = paste("window.open('", data$link[1], "', '_blank', ''); return false;", sep = "")),
                  "highlight" = list(onmouseover = "highlight(evt)", onmouseout = "downlight(evt)")
                  )
    args <- c(args, arg)
  }
  do.call(garnishGrob, args)
}

add_interactivity.gTree <- add_interacticity.polyline

add_interactivity.rect <- function (gr, data) {
    
}

add_interactivity.zeroGrob <- function(gr, data) {
  return(gr)
}