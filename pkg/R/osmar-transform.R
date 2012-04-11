###########################################################
## Transform osmar objects to plotDataFrame
#########################################################

##' ##' This function takes the attributes of an osmar object and converts them into
##' the key-value form, the osmar tags are saved in for later binding together with them.
##'
##' .. content for \details{} ..
##' @title Converts osmar attrs to tags
##' @param osmar_obj_attrs Attribute data.frame from an element of an osmar object (node, way or relation)
##' @param var # Variable which is wanted for later plotting
##' @return Merged and melted data.frame in the form: id, key, value
##' @author chris
melt_attrs <- function(attrs, vars){
  # columns to keep
  columns <- c("id", vars)
  # discard other
  columns <- columns[columns %in% names(attrs)]
  if ( length(columns) == 1 ) return(data.frame(id = numeric(0), variable = character(0), value = character(0)))
  tmp <- melt(attrs[columns], id.vars = c("id"), variable_name = "variable", na.rm=TRUE)
  tmp
}

##' Function merges attrs and tags of an osmar-element with a subset for var
##'
##' 
##' @title Merge Attributes and Tags
##' @param osmar_elem 
##' @param var 
##' @return dataframe containing attrs and tags in key-value form
##' @author chris
merge_attrs_tags <- function(osmar_elem, vars){
  attrs <- osmar_elem$attrs
  attrs_melted <- melt_attrs(attrs, vars)
  # other formats than character (e.g. posixT) can throw errors
  attrs_melted$value <- as.character(attrs_melted$value)
  
  # tags
  tags <- osmar_elem$tags
  tags <- rename(tags, c(k = "variable", v = "value"))

  # bind together
  rbind(attrs_melted, tags)
}


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
merge_coords_attrs <- function(coords, attrs, vars){
  # discard other keys
  attrs_sub <- subset(attrs, subset = ( variable %in% vars ), drop=TRUE)
  attrs_sub <- rename(attrs_sub, c(id = "element_id"))
  # merge coords and attributes
  all <- merge(coords, attrs_sub, all.x = FALSE, sort = FALSE)
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
  
  coords <- rename(coords, c(id = "node_id"))
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
  ways <- osmar_obj$ways

  # get the coords from all nodes
  coords <- get_coords_nodes(osmar_obj)[c("node_id", "lat", "lon")]
  coords$element_id <- NULL
  # and the references 
  refs <- ways[["refs"]]
  ## to be able to sort it later in the right way;
  refs$order <- seq_len(nrow(refs))
  refs <- rename(refs, c(ref = "node_id"))
  ways_coords <- merge(refs, coords, by="node_id")
  geom <- ifelse(nrow(ways_coords) > 0, "path", character(0))
  ways_coords$geom <- geom
  ways_coords <- rename(ways_coords, c(ref = "node_id", id = "element_id"))
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


##' Merges the attributes and tags of nodes with a subset of
##' the key variable. 
##'
##' content
##' @title Melt nodes
##' @param osmar_obj an osmar object
##' @param var the subset will be made with this variable
##' @return dataframe containing coordinates and attributes
##' @author chris
melt_nodes<- function(osmar_obj, vars){
  nodes <- osmar_obj$nodes
  # merge attrs and tags
  tags  <- merge_attrs_tags(nodes, vars)
  coords <- get_coords_nodes(osmar_obj)
  merge_coords_attrs(coords, tags, vars = vars)
}



# possible improvements: only take subset for ways_coords
##' Merges attributes and coordinates of ways. 
##'
##' .. content for \details{} ..
##' @title Melt ways
##' @param osmar_obj an osmar object
##' @param var string with the name of the keys to keep
##' @return a dataframe with coordinates and attributes
##' @author chris
melt_ways <- function(osmar_obj, vars_poly, vars_path){
  ways <- osmar_obj[["ways"]]
  ## merge attrs and tags in long format for lines
  coords <- get_coords_ways(osmar_obj)
  attrs <- merge_attrs_tags(ways, c(vars_path, vars_poly))
  ways2 <- merge_coords_attrs(coords, attrs, c(vars_path, vars_poly))
  ## delete all rows were geom and vars do not fit together
  ways2[which((( ways2$geom == "path" ) &  ( ways2$variable %in% vars_path ) ) |
               (( ways2$geom == "polygon") & ( ways2$variable %in% vars_poly ) )),]
}


# merges attrs and tags of relations and resolves location through ways, relations and nodes
##' Merges coordinates and attributes for relations
##'
##' A relation consists possibly of nodes, ways and relations.
##' The relations will get resolved recursevily. The information for the nodes and ways
##' will be fetched with the nodes and ways- specific functions.
##' @title Melt relations
##' @param osmar_obj an osmar object
##' @param var a string with the name of the keys to keep
##' @return data.frame with ways and nodes of relation
##' @author chris
melt_relations <- function(osmar_obj, vars){
  relations <-  osmar_obj[["relations"]]
  attrs <- merge_attrs_tags(relations, vars)
  coords <- get_coords_relations(osmar_obj)
  refs <- relations$refs[,c("id", "ref")]
  refs <- rename(refs, c(ref = "element_id"))
  refs_with_infos <- merge(attrs, refs, by = "id")
  refs_with_infos <- subset(refs_with_infos, subset = ( refs_with_infos$variable == vars ))
  res <- merge(refs_with_infos, coords)
  res$id <- NULL
  res
}



##' Converts an osmar object into a data.frame long format for plotting
##'
##' .. content for \details{} ..
##' @title Melt osmar
##' @param osmar_obj an osmar object
##' @param node.vars A character vector containing the desired node variable names
##' @param way.vars A character vector containing the desired way variable names
##' @param relation.vars A character vector containing the desired relation variable names
##' @return data.frame in long format containing element_id, node_id, key, value, lat, lon, geom
##' @author chris
fortify_osmar <- function(osmar_obj, vars_node=NA, vars_path=NA, vars_poly=NA, vars_relations=NA){
  nodes <- melt_nodes(osmar_obj, var = vars_node)
  ways <- melt_ways(osmar_obj, vars_path = vars_path, vars_poly = vars_poly)
  # for performance issues this could take place at an earlier time
  relations  <- melt_relations(osmar_obj, vars = vars_relations)
  res <- rbind(nodes, ways, relations)
  # do this at another place
  res <- res[which(!is.na(res$variable)), ]
  # Do this at another place? if i use join from Had.Whick. maybe not necessary.
  #res <- res[order(res$element_id, res$order),]
  res
}

##' Changes the geom of ways from path to geom if first and last nodes coordinates are the same
##'
##' .. content for \details{} ..
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
