### adding interactivity to a grob ############################################
def_iaes <- aes(tooltip = "", link = NA, show = 0)



escape_xml <- function (string) {
  dic <- c( "&" = "&amp;", 
            "<" = "&lt;", 
            ">" = "&gt;",
            '\"' = "&quot;", 
            "\'" = "&apos;")
  for (d in seq_along(dic)){
    string <- gsub(names(dic)[d], dic[d], string)
  }
  string
}



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
  inter <- intersect(c("tooltip", "highlight", "link", "show"), names(data))
  data[inter] <- apply(data[inter], 2, escape_xml)
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
  data[inter] <- apply(data[inter], 2, escape_xml)
  args <- list(x = gr, group = FALSE)
  for (interact in inter) {
    arg <- switch(interact,
                  "tooltip" = list(tooltip = data$tooltip),
                  "link" = list(link = data$link),
                  "highlight" = list(highlight = data$highlight),
                  "show" = list(show = data$show)
                  )
    args <- c(args, arg)
  }
  igr <- do.call(gridSVG::garnishGrob, args)
  igr
}



add_interactivity.grob <- function (gr, data) {
  data[inter] <- apply(data[inter], 2, escape_xml)
  args <- list(x = gr, group = FALSE)
  for (interact in inter) {
    arg <- switch(interact,
                  "tooltip" = list(tooltip = data$tooltip[1]),
                  "link" = list(link = data$link[1]),
                  "highlight" = list(highlight = data$highlight[1]),
                  "show" = list(show = data$show[1])
                  )
    args <- c(args, arg)
  }
  do.call(gridSVG::garnishGrob, args)
}
