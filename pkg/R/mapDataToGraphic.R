
##' Checks if Mapping is valid for Element
##'
##' This function returns TRUE, if the mapping method is
##' valid for the element. What is allowed is mainly dictated by the
##' aesthetics options for the fitting geom from ggplot2
##' @title 
##' @param element Graphical Element, e.g. lines, points, ...
##' @param mapping Graphical Attributes, e.g. colour, toolTip, ..
##' @return TRUE if mapping is valid for element, else FALSE
##' @author chris
checkMapping <- function(element, mapping){
  valid_for_all <- c("colour", "toolTip", "alpha")
  valid_mapp <- switch(element,
                       line=c("shape", "size"),
                       point=c("shape", "size"),
                       polygon=c("linetype", "size"),
                       grid=c("fill", "linetype", "size")
                       )
  valid_mapp <- c(valid_mapp, valid_for_all)
  mapping %in% valid_mapp
}



getData <- function(object, points, lines, polygons, grid){
  if(osmar:::is_osmar(object)){
    data <- osmar_long(object, node.vars=points, way.vars=c(lines, polygons)) # and relations????
  } else if(attributes(class(object)) == "sp"){
                                        # HOW SHOULD THE VARS BE TREATED???
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
  points.vars <- unlist(points)
  lines.vars <- unlist(lines)
  polygons.vars <- unlist(polygons)
  grid.vars <- unlist(grid)
  
  # check mapping
  df <- getData(object, points.vars, lines.vars, polygons.vars, grid.vars)
  df <- cast(df, ...~variable)
  df <- df[order(df$element_id, df$order),]
  # get dataframe out of objects
  # MISSSING result: df
  

  ## cast the datafram

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
