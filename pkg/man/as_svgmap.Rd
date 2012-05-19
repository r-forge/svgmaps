\name{as_svgmap}
\alias{as_svgmap}
\title{as_svgmap}
\usage{
  as_svgmap(object, ...)
}
\arguments{
  \item{object}{Spatial objects; Currently osmar and
  SpatialXXXDataFrame objects}

  \item{...}{Not yet in use}
}
\value{
  A svgmap_df data frame
}
\description{
  Generic function which transforms spatial objects into
  the svgmap_df data frame format.
}
\details{
  Works like fortify This function takes spatial data
  (currently sp and osmar) and transforms it into a
  suitable way for the svgmaps package, which is a special
  data frame. For SpatialLinesDataFrame and
  SpatialPolygonsDataFrame it is assumed, that the ids are
  the rownames.
}
\author{
  chris
}

