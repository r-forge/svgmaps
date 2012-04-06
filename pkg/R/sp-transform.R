
# MAKE THIS MORE PRETTY
spatialPointsDataFrame_long <- function(sp_points_df, vars){
  df <- cbind(sp_points_df@data, coordinates(sp_points_df))
  df_sub <- df[c("x", "y", vars)]
  df_sub <- rename(df_sub, c(x="lon", y="lat"))
  df_sub$order <- seq_len(nrow(df_sub))
  df_melted <- melt(df_sub, id.vars=c("lon", "lat"))
  df_melted$geom <- "point"
  df_melted$element_id <- seq_len(nrow(df_melted))
  df_melted$point_id <- df_melted$element_id
  df_melted
}


spatialLinesDataFrame_long <- function(sp_lines_df, vars){
  # extract the data
  dat <- sp_lines_df@data
  dat <- dat[,vars, drop=FALSE]
  dat$id <- rownames(dat)
  coords <- fortify(sp_lines_df)
  
  df <- join(coords, dat, by="id")
  df <- rename(df, c(long="lon", group="element_id"))
    
  # Id and piece are not needed any more, so drop them
  df$id <- NULL
  df$piece <- NULL
  
  # add point_id and geom
  df$point_id <- seq_len(nrow(df))
  df$geom <- "line"

  # melt and subset
  df_m <- melt(df, id.vars=c("element_id", "lon", "lat", "point_id", "order"))
  df_m
}


# maybe change the order of subsetting and melting also in spatialLines
# maybe use fortify.SpatialPolygonsDataFrame {ggplot2} to change this
## assumes that rownames() of data is the id
spatialPolygonsDataFrame_long <- function(sp_polygons_df, vars){
  # extract the data
  sp_polygons_df@data$id <- rownames(sp_polygons_df@data)
  dat <- sp_polygons_df@data
  dat <- dat[,c("id", vars),drop=FALSE]
  coords <- fortify(sp_polygons_df, region="id")
  # coords$id <- factor(as.character(coords$id), labels=seq_len(nrow(dat)))
  df <- join(dat, coords, by="id")
  df <- rename(df, c(group="element_id", long="lon"))
  ## Id is not needed any more, so drop it
  df$id <- NULL
  df$point_id <- seq_len(nrow(df))
  df$geom <- "polygon"
  df_m <- melt(df, measure.vars=vars)
  df_m
}

spatialPixelsDataFrame_long <- function(sp_pixels_df, vars){
  df <- as.data.frame(sp_pixels_df)
  df <- df[,c("x","y",vars)]
  df$geom <- "tile"
  df$order <- 1
  df <- rename(df, c(x="lon", y="lat"))
  df$element_id <- seq_len(nrow(df))
  df$node_id <- df$element_id
  melt(df, measure.vars=vars)
}


spatialGridDataFrame_long <- function(sp_grid_df, vars){
  spatialPixelsDataFrame_long(sp_grid_df, vars)
}

sp_long <- function(sp_object, p, l, poly, g){
  todo <- switch(class(sp_object),
                 "SpatialPointsDataFrame"= list(fun=spatialPointsDataFrame_long, vars=p),
                 "SpatialLinesDataFrame"=list(fun=spatialLinesDataFrame_long, vars=l),
                 "SpatialPolygonsDataFrame"=list(fun=spatialPolygonsDataFrame_long, vars=poly),
                 "SpatialPixelsDataFrame"=list(fun=spatialPixelsDataFrame_long, vars=g),
                 "SpatialGridDataFrame"=list(fun=spatialGridDataFrame_long, vars=g)
                 )
  do.call(todo$fun, args=list(sp_object, vars=todo$vars))
}
