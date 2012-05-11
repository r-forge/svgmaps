\name{change_ways2polygons}
\alias{change_ways2polygons}
\title{ways2Polygons}
\usage{
  change_ways2polygons(ways_long)
}
\arguments{
  \item{ways_long}{A dataframe containing the way
  coordinates and an id variable}
}
\value{
  A dataframe with changed geom column
}
\description{
  Changes the geom of ways from path to geom if first and
  last nodes coordinates are the same
}
\details{
  It is assumed that all ways which first node equals the
  last node can be interpretated as polygons. The other
  will be paths.
}
\author{
  chris
}

