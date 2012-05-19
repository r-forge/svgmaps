\name{svgmap}
\alias{svgmap}
\title{svgmap}
\usage{
  svgmap(data = NULL, ...)
}
\arguments{
  \item{data}{The data, can currently be a
  SpatialXXXDataFrame or an osmar object}

  \item{...}{Not in use}
}
\value{
  An object of with class svgmap and ggplot. Can be used
  like a normal ggplot2 object, but also with the functions
  view_svgmap and save_svgmap.
}
\description{
  ##' ggplot() like function for interactive maps
}
\details{
  This function can be used similiar to ggplot(). It is
  limited to input objects which can be handled by
  as_svgmap. Currently these are objects with class
  SpatialXXXDataFrame, osmar and svgmap_df.
}
\author{
  chris
}

