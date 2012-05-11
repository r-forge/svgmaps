\name{as_svgmap.SpatialGridDataFrame}
\alias{as_svgmap.SpatialGridDataFrame}
\title{as_svgmaps.SpatialGridDataFrame}
\usage{
  as_svgmap.SpatialGridDataFrame(object, ...)
}
\arguments{
  \item{sp_grid_df}{A SpatialGridDataFrame object}

  \item{vars}{Variables to be kept}
}
\value{
  Data Frame in svgmaps format
}
\description{
  Transforms SpatialGridDataFrame into svgmaps data frame
}
\details{
  This function internally uses the
  as_svgmaps.SpatialPixelsDataFrame function
}
\author{
  chris
}

