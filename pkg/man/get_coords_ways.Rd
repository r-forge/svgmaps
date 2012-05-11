\name{get_coords_ways}
\alias{get_coords_ways}
\title{Coordinates of ways}
\usage{
  get_coords_ways(osmar_obj)
}
\arguments{
  \item{osmar_obj}{An osmar object}
}
\value{
  An data.frame containing id_element, id_node, lat, lon,
  geom
}
\description{
  Get the coordinates for ways
}
\details{
  This function greps the information of lon and lat from a
  given osmar object. Therefor it merges the ways$refs with
  the nodes$attrs
}
\author{
  chris
}

