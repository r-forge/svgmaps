\name{as_svgmap.SpatialPolygonsDataFrame}
\alias{as_svgmap.SpatialPolygonsDataFrame}
\title{as_svgmap.SpatialPolygonsDataFrame}
\usage{
  \method{as_svgmap}{SpatialPolygonsDataFrame} (object,
    ...)
}
\arguments{
  \item{object}{A SpatialPolygonsDataFrame object}

  \item{...}{Not in use}
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

