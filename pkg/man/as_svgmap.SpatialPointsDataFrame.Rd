\name{as_svgmap.SpatialPointsDataFrame}
\alias{as_svgmap.SpatialPointsDataFrame}
\title{as_svgmaps.SpatialPointsDataFrame}
\usage{
  as_svgmap.SpatialPointsDataFrame(object, ...)
}
\arguments{
  \item{sp_points_df}{A SpatialPointsDataFrame object}

  \item{vars}{Variables to be kept}
}
\value{
  Data frame in svgmaps format
}
\description{
  Transfroms SpatialPointsDataFrame into svgmaps data frame
}
\details{
  This function merges the coordinates and the data of a
  SpatialPointsDataFrame. The function assumes, that
  coordinates are called x and y or lon and lat.
}
\author{
  chris
}

