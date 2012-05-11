\name{add_keys}
\alias{add_keys}
\title{add_keys}
\usage{
  add_keys(osmar, keys)
}
\arguments{
  \item{osmar}{An osmar object}

  \item{keys}{A character vector}
}
\value{
  An osmar object with enhanced attributes data frame
}
\description{
  Add keys to an the attributes from an osmar object
}
\details{
  This function takes an osmar object and a set of keys
  (character vector) and then looks up the keys in the tags
  data frame and adds them to the attrs data frame. This
  happens for all nodes, ways and relations.
}
\author{
  chris
}

