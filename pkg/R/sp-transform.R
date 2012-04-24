

##' Transfroms SpatialPointsDataFrame into svgmaps data frame
##'
##' This function merges the coordinates and the data of a SpatialPointsDataFrame.
##' The function assumes, that coordinates are called x and y or lon and lat.
##' @title as_svgmaps.SpatialPointsDataFrame
##' @param sp_points_df A SpatialPointsDataFrame object
##' @param vars Variables to be kept
##' @return Data frame in svgmaps format
##' @author chris
as_svgmaps.SpatialPointsDataFrame <- function(sp_points_df){
  df <- cbind(sp_points_df@data, coordinates(sp_points_df))
  df_sub <- rename(df_sub, c(x = "lon", y = "lat"))
  df$order <- 1
  df$geom <- "point"
  df$element_id <- seq_len(nrow(df))
  df$point_id <- df$element_id
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
##' @author chris
as_svgmaps.SpatialLinesDataFrame <- function(sp_lines_df){
  # data is in data slot
  dat <- sp_lines_df@data
  # rownames are always treated as id
  dat$id <- rownames(dat)
  coords <- fortify(sp_lines_df)
  
  df <- join(coords, dat, by="id")
  df <- rename(df, c(long="lon", group="element_id"))
    
  # Id and piece are not needed any more, so drop them
  df$id <- NULL
  df$piece <- NULL
  
  # add point_id and geom
  df$point_id <- seq_len(nrow(df))

  df$geom <- "path"
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
##' @author chris
as_svgmaps.SpatialPolygonsDataFrame <- function(sp_polygons_df){
  # extract the data
  sp_polygons_df@data$id <- rownames(sp_polygons_df@data)
  dat <- sp_polygons_df@data
  coords <- fortify(sp_polygons_df, region="id")
  # coords$id <- factor(as.character(coords$id), labels=seq_len(nrow(dat)))
  df <- join(dat, coords, by="id")
  df <- rename(df, c(group="element_id", long="lon"))
  ## Id is not needed any more, so drop it
  df$id <- NULL
  df$point_id <- seq_len(nrow(df))
  df$geom <- "polygon"
  df
}
##' Transforms SpatialPixelsDataFrame into svgmaps data frame
##'
##' 
##' @title as_svgmaps.SpatialPixelsDataFrame
##' @param sp_pixels_df A SpatialPixelsDataFrame object
##' @param vars Variables to be kept
##' @return Data frame in svgmaps format
##' @author chris
as_svgmaps.SpatialPixelsDataFrame <- function(sp_pixels_df){
  df <- as.data.frame(sp_pixels_df)
  df$geom <- "tile"
  df$order <- 1
  df <- rename(df, c(x="lon", y="lat"))
  df$element_id <- seq_len(nrow(df))
  df$node_id <- df$element_id
  df
}

##' Transforms SpatialGridDataFrame into svgmaps data frame
##'
##' This function internally uses the as_svgmaps.SpatialPixelsDataFrame function
##' @title as_svgmaps.SpatialGridDataFrame
##' @param sp_grid_df A SpatialGridDataFrame object
##' @param vars Variables to be kept
##' @return Data Frame in svgmaps format
##' @author chris
as_svgmaps.SpatialGridDataFrame <- function(sp_grid_df){
  melt_pixels(sp_grid_df)
}

