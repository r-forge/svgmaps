\name{as_svgmap.osmar}
\alias{as_svgmap.osmar}
\title{Melt osmar}
\usage{
  as_svgmap.osmar(object, keys = NULL, ...)
}
\arguments{
  \item{osmar_obj}{An osmar object}

  \item{node.vars}{A character vector containing the
  desired node variable names}

  \item{way.vars}{A character vector containing the desired
  way variable names}

  \item{relation.vars}{A character vector containing the
  desired relation variable names}
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

