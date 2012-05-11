\name{melt_relations}
\alias{melt_relations}
\title{Melt relations}
\usage{
  melt_relations(osmar_obj, vars)
}
\arguments{
  \item{osmar_obj}{an osmar object}

  \item{var}{a string with the name of the keys to keep}
}
\value{
  data.frame with ways and nodes of relation
}
\description{
  Merges coordinates and attributes for relations
}
\details{
  A relation consists possibly of nodes, ways and
  relations. The relations will get resolved recursevily.
  The information for the nodes and ways will be fetched
  with the nodes and ways- specific functions.
}
\author{
  chris
}

