\name{get_coords_nodes}
\alias{get_coords_nodes}
\title{Coordinates of nodes}
\usage{
  get_coords_nodes(osmar_obj)
}
\arguments{
  \item{osmar_obj}{An osmar object}
}
\value{
  a data.frame with columns id, lat, lon, geom
}
\description{
  Get the coordinates of all nodes
}
\details{
  You put in a object of class osmar. This function takes
  the id_element, id_node, lat and lon columns from
  nodes$attrs
}
\author{
  chris
}

