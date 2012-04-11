
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



get_data <- function(object, points, lines, polygons, grid){
  if(osmar:::is_osmar(object)){
    data <- fortify_osmar(object, vars_node=points, vars_path=lines, vars_poly= polygons) # and relations????
  } else if(attributes(class(object)) == "sp"){
    ## HOW SHOULD THE VARS BE TREATED???
    data <- sp_long(object, p=points, l=lines, poly=polygons, g=grid)
  } else if(class(object) == "list"){
    # ADD THE VARIABLES 
    data <- ldply(object, .fun = sp_long)
  } else {
    stop("The object has to be an osmar object, a sp object or a list containing sp objects")
  }
}




  

svgmap <- function(object,
                   points=list(colour=NULL, size=NULL),
                   lines=list(),
                   polygons=list(),
                   grid=list(),
                   bbox
                   ){
  
  # bring variables list in a convenient form
  # MISSING
  vars_points <- unlist(points)
  vars_lines <- unlist(lines)
  vars_polygons <- unlist(polygons)
  vars_grid <- unlist(grid)
  
  # check mapping

  # get data out of object in long format
  df <- get_data(object, vars_points, vars_lines, vars_polygons, vars_grid)
  
  # convert to wide format
  df <- cast(df, ...~variable)
  df <- df[order(df$element_id, df$order),]
  
  # produce layers
  layers <- dlply(df, .(geom),
                  .fun=function(X){
                    ## which geom to use?
                    geom <- unique(X$geom)
                    mapping <- switch(geom,
                                      "point"=points,
                                      ## better: change everything to path
                                      "path"=lines,
                                      "polygon"=polygons,
                                      "tile"=grid)
                    ## do the mapping
                    mapping <- c(mapping, list(x="lon", y="lat", group="element_id"))
                    mapping <- do.call(aes_string, mapping)
                    layer(geom=geom, data=X, mapping=mapping)
                  }
                )
  ## MAYBE there is a better solution with Reduce
  p <- ggplot() + coord_map()
  for(i in seq_len(length(layers))){
    p <- p + layers[[i]]
  }
  print(p)
    
  
 
  ## add_interactivity
  ## make svg
}


get_bbox <- function(object){
  if ("osmar" %in% class(object)) cbind(lat = range(object$nodes$attrs$lat), lon = range(object$nodes$attrs$lon)) else
  sp::bbox(object)
}
#get_bbox(home.sp$points)
