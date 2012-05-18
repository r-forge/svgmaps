

##' Transfroms SpatialPointsDataFrame into svgmaps data frame
##'
##' This function merges the coordinates and the data of a SpatialPointsDataFrame.
##' The function assumes, that coordinates are called x and y or lon and lat.
##' @title as_svgmaps.SpatialPointsDataFrame
##' @param sp_points_df A SpatialPointsDataFrame object
##' @param vars Variables to be kept
##' @return Data frame in svgmaps format
##' @author chris
as_svgmap.SpatialPointsDataFrame <- function(object, ...){
  df <- cbind(object@data, coordinates(object))
  df <- rename(df, c(x = "lon", y = "lat"))
  df$order <- 1
  df$geom <- "point"
  df$element_id <- seq_len(nrow(df))
  df$point_id <- df$element_id
  class(df) <- c("svgmap_df", class(df))
  df
}

##' Transforms SpatialLinesDataFrame into svgmaps data frame
##'
##' This function merges the coordinates and the data of a
##' SpatialLinesDataFrame
##' @title as_svgmaps.SpatialLinesDatFrame
##' @param sp_lines_df A SpatialLinesDataFrame object
##' @param vars Variables to be kept
##' @return Data frame in svgmaps format
##' @export
##' @author chris
as_svgmap.SpatialLinesDataFrame <- function(object, ...){
  # data is in data slot
  dat <- object@data
  # rownames are always treated as id
  dat$id <- rownames(dat)
  coords <- fortify(object)
  
  df <- join(coords, dat, by="id")
  df <- rename(df, c(long="lon", group="element_id"))
    
  # Id and piece are not needed any more, so drop them
  df$id <- NULL
  df$piece <- NULL
  
  # add point_id and geom
  df$point_id <- seq_len(nrow(df))

  df$geom <- "path"
  class(df) <- c("svgmap_df", class(df))
  df
}


##' Transforms SpatialPolygonsDataFrame into svgmaps data frame
##'
##' The coordinates are transformed using ggplot's fortify function
##' The data is merged than with the coordinates. This function assumes
##' that the rownames of the data frame are the ids of the polygons.
##' @title as_svgmaps.SpatialPolygonsDataFrame
##' @param sp_polygons_df A SpatialPolygonsDataFrame object
##' @param vars Variables to be kept
##' @return Data frame in svgmaps format
##' @export
##' @author chris
as_svgmap.SpatialPolygonsDataFrame <- function(object, ...){
  # extract the data
  object@data$id <- rownames(object@data)
  dat <- object@data
  coords <- fortify(object, region="id")
  # coords$id <- factor(as.character(coords$id), labels=seq_len(nrow(dat)))
  df <- join(dat, coords, by="id")
  df <- rename(df, c(group="element_id", long="lon"))
  ## Id is not needed any more, so drop it
  df$id <- NULL
  df$point_id <- seq_len(nrow(df))
  df$geom <- "polygon"
  class(df) <- c("svgmap_df", class(df))
  df
}
##' Transforms SpatialPixelsDataFrame into svgmaps data frame
##'
##' 
##' @title as_svgmaps.SpatialPixelsDataFrame
##' @param sp_pixels_df A SpatialPixelsDataFrame object
##' @param vars Variables to be kept
##' @return Data frame in svgmaps format
##' @export
##' @author chris
as_svgmap.SpatialPixelsDataFrame <- function(object, ...){
  df <- as.data.frame(object)
  names(df)[(ncol(df) - 1):ncol(df)] <- c("lon", "lat")
  df$geom <- "tile"
  df$order <- 1
  df$element_id <- seq_len(nrow(df))
  df$point_id <- df$element_id
  class(df) <- c("svgmap_df", class(df))
  df
}

##' Transforms SpatialGridDataFrame into svgmaps data frame
##'
##' This function internally uses the as_svgmaps.SpatialPixelsDataFrame function
##' @title as_svgmaps.SpatialGridDataFrame
##' @param sp_grid_df A SpatialGridDataFrame object
##' @param vars Variables to be kept
##' @return Data Frame in svgmaps format
##' @export
##' @author chris
as_svgmap.SpatialGridDataFrame <- function(object, ...){
  # transform object to SpatialPixelsDataFrame
  as_svgmap.SpatialPixelsDataFrame(object, ...)
##  NextMethod("as_svgmap")
}

