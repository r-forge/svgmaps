fortify.SpatialPointsDataFrame <- function (model, data, id = seq_along(model), ...) {
  df <- as.data.frame(coordinates(model))
  df <- rename(df, c(x = "long", y = "lat"))
  df$id <- id
  df
}


fortify.SpatialGridDataFrame <- function (model, data, id, ...) {
  as.data.frame(model)
}


fortify.SpatialPixelsDataFrame <- function(model, data, id, ...) {
  fortify.SpatialGridDataFrame(model = model, data = data, id = id, ...)
}

head(fortify(meuse.grid))