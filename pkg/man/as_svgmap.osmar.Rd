\name{as_svgmap.osmar}
\alias{as_svgmap.osmar}
\title{Melt osmar}
\usage{
  \method{as_svgmap}{osmar} (object, keys = NULL, ...)
}
\arguments{
  \item{object}{An osmar object}

  \item{keys}{Character string determining keys to keep}

  \item{...}{Not in use}
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

