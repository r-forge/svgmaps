\name{as_svgmap.SpatialPolygonsDataFrame}
\alias{as_svgmap.SpatialPolygonsDataFrame}
\title{as_svgmaps.SpatialPolygonsDataFrame}
\usage{
  as_svgmap.SpatialPolygonsDataFrame(object, ...)
}
\arguments{
  \item{sp_polygons_df}{A SpatialPolygonsDataFrame object}

  \item{vars}{Variables to be kept}
}
\value{
  Data frame in svgmaps format
}
\description{
  Transforms SpatialPolygonsDataFrame into svgmaps data
  frame
}
\details{
  The coordinates are transformed using ggplot's fortify
  function The data is merged than with the coordinates.
  This function assumes that the rownames of the data frame
  are the ids of the polygons.
}
\author{
  chris
}

