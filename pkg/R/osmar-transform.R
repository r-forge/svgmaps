###########################################################
## Transform osmar objects to plotDataFrame
#########################################################


##' Merge the coordinates and attributes of nodes or ways
##'
##' This function takes the tags dataframe, where the attributes dataframe is added.
##' It then merges both dataframes by the id of the element.
##' It also makes a subset, i.e. also the rows with key=vars will be kept.
##' A geom column will be added. This can be one of line or point.
##' @title Merge coordinates and attributes
##' @param coords dataframe with id and coordinates
##' @param attrs dataframe with id and attributes in key-value form
##' @param vars keys to keep 
##' @return data.frame with coordinates and attributes in key-value form 
##' @author chris
merge_coords_attrs <- function(coords, attrs){
  attrs <- rename(attrs_sub, c(id = "element_id"))
  # merge coords and attributes
  all <- merge(coords, attrs_sub, all.x = TRUE, sort = FALSE)
  #all <- join(coords, attrs_sub,by="element_id", type="inner")
  all
}

 


##' Get the coordinates of all nodes 
##'
##' You put in a object of class osmar. This function takes the id_element, id_node, lat and lon columns
##' from nodes$attrs
##' 
##' @title Coordinates of nodes
##' @param osmar_obj An osmar object
##' @return a data.frame with columns id, lat, lon, geom
##' @author chris
get_coords_nodes <- function(osmar_obj){
  nodes <- osmar_obj$nodes
  coords <- nodes$attrs[c("id", "lat", "lon")]
  coords$element_id <- coords$id
  
  # in case the df is empty
  geom <- ifelse(nrow(coords) > 0, "point", character(0))
  order <- ifelse(nrow(coords) > 0, 1, numeric(0))
  coords$geom <- geom
  coords$order <- order
  
  coords <- rename(coords, c(id = "point_id"))
  coords
}


##' Get the coordinates for ways 
##'
##' This function greps the information of lon and lat from a given
##' osmar object. Therefor it merges the ways$refs with the nodes$attrs
##' 
##' @title Coordinates of ways
##' @param osmar_obj An osmar object
##' @return An data.frame containing id_element, id_node, lat, lon, geom
##' @author chris
get_coords_ways <- function(osmar_obj){
  # browser()
  ways <- osmar_obj$ways

  # get the coords from all nodes
  coords <- get_coords_nodes(osmar_obj)[c("point_id", "lat", "lon")]
  coords$element_id <- NULL
  # and the references 
  refs <- ways[["refs"]]
  ## to be able to sort it later in the right way;
  refs$order <- seq_len(nrow(refs))
  refs <- rename(refs, c(ref = "point_id"))
  ways_coords <- merge(refs, coords, by="point_id")
  geom <- "path"
  if(nrow(ways_coords) ==  0) geom <- character(0)
  ways_coords$geom <- geom
  ways_coords <- rename(ways_coords, c(ref = "point_id", id = "element_id"))
  # overwrite line with polygon in geom, if first and last coordinate fit
  ways_coords <- change_ways2polygons(ways_coords)
  ways_coords
}


#' Gets the coordinates for relations
## Only works for depth=1, refs on other relations will not be resolved
#' missing: identifier for the relation
get_coords_relations <- function(osmar_obj){
  # get relations
  relations <- osmar_obj$relation

  # nodes coords of relations
  rel_node_ids <-unique(relations$refs[relations$ref$type == "node",])$ref
  nodes_coords <- get_coords_nodes(osmar_obj)
  nodes_coords_rel <- subset(nodes_coords, subset=(nodes_coords$element_id %in% rel_node_ids))

  # ways coords of relations
  rel_way_ids <- unique(relations$refs[relations$refs$type == "way",])$ref
  ways_coords <- get_coords_ways(osmar_obj)
  ways_coords_rel <- subset(ways_coords, subset=(ways_coords$element_id %in% rel_way_ids))
  rbind(nodes_coords_rel, ways_coords_rel)
}



##' Converts an osmar object into a data.frame long format for plotting
##'
##' This function merges the coordinates with the data in an osmar object.
##' It is done for nodes, ways (which will be splitted into paths and polygons), but not
##' for relations as they are to complex.
##' @title Melt osmar
##' @param osmar_obj An osmar object
##' @param node.vars A character vector containing the desired node variable names
##' @param way.vars A character vector containing the desired way variable names
##' @param relation.vars A character vector containing the desired relation variable names
##' @return data.frame in long format containing element_id, node_id, key, value, lat, lon, geom
##' @S3method as_svgmap osmar
##' @author chris
as_svgmap.osmar <- function(object, keys = NULL, ...){
  object <- add_keys(object, keys)
  nodes <- melt_nodes(object, ...)
  ways <- melt_ways(object, ...)
  # relations  <- melt_relations(osmar_obj, vars = vars_relations)
  res <- rbind(nodes, ways)
  res <- res[order(res$element_id, res$order), ]
  class(res) <- c("svgmap_df", class(res))
  res
}

##' Changes the geom of ways from path to geom if first and last nodes coordinates are the same
##'
##' It is assumed that all ways which first node equals the last node can be interpretated
##' as polygons. The other will be paths.
##' @title ways2Polygons
##' @param ways_long A dataframe containing the way coordinates and an id variable
##' @return A dataframe with changed geom column
##' @author chris
change_ways2polygons <- function(ways_long){
  ddply(ways_long,
        .(element_id),
        function(x){
          first <- x[x$order == min(x$order), c("lon", "lat")]
          last <- x[x$order == max(x$order), c("lon", "lat")]
           geom <- unique(x$geom)
          if (all(first == last) & geom == "path"){
            x$geom <- "polygon"
          }
          x
        }
        )
}



##' Add keys to an the attributes from an osmar object
##'
##' This function takes an osmar object and a set of keys (character vector) and then
##' looks up the keys in the tags data frame and adds them to the attrs data frame.
##' This happens for all nodes, ways and relations.
##' 
##' @title add_keys
##' @param osmar An osmar object
##' @param keys A character vector
##' @return An osmar object with enhanced attributes data frame
##' @author chris
add_keys <- function(osmar, keys) {
  stopifnot(osmar:::is_osmar(osmar))
  if (is.null(keys)) return(osmar)
  for (type in c("nodes", "ways", "relations")){
    # subset osmar object
    df <- osmar[[type]]
    # choose tags and cast them
    tags <- subset(osmar[[type]]$tags, subset = k %in% keys)
    tags <- dcast(tags, id ~ k, value.var = "v")
    # delete NA columns
    if (any(names(tags) %in% "NA")){
      tags <- tags[, -which(names(tags) == "NA"), drop= FALSE]
    }
     browser()
    # add missing columns with NA
    if (nrow(tags) == 0) {
      for (k in setdiff(keys, names(tags))){
        tags[k] <- character(0)
      }
    } else {
      tags[setdiff(keys, names(tags))] <- NA
    }
    osmar[[type]]$attrs <-  merge(osmar[[type]]$attrs, tags, by = "id", all = TRUE)
  }
  osmar
}



