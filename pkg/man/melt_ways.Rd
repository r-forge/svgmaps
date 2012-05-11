\name{melt_ways}
\alias{melt_ways}
\title{Melt ways}
\usage{
  melt_ways(osmar_obj)
}
\arguments{
  \item{osmar_obj}{an osmar object}

  \item{var}{string with the name of the keys to keep}
}
\value{
  a dataframe with coordinates and attributes
}
\description{
  Merges attributes and coordinates of ways.
}
\details{
  This merges the coords and data of ways. Ways are split
  up into paths and polygons.
}
\author{
  chris
}

