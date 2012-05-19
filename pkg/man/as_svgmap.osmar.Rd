\name{as_svgmap.osmar}
\alias{as_svgmap.osmar}
\title{Melt osmar}
\usage{
  as_svgmap.osmar(object, keys = NULL, ...)
}
\arguments{
  \item{osmar_obj}{An osmar object}

  \item{keys}{Character string with the keys which shall be
  included in the svgmap_df}
}
\value{
  data.frame in long format containing element_id, node_id,
  key, value, lat, lon, geom
}
\description{
  Converts an osmar object into a data.frame long format
  for plotting
}
\details{
  This function merges the coordinates with the data in an
  osmar object. It is done for nodes, ways (which will be
  splitted into paths and polygons), but not for relations
  as they are to complex.
}
\author{
  chris
}

