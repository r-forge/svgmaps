\name{merge_coords_attrs}
\alias{merge_coords_attrs}
\title{Merge coordinates and attributes}
\usage{
  merge_coords_attrs(coords, attrs)
}
\arguments{
  \item{coords}{dataframe with id and coordinates}

  \item{attrs}{dataframe with id and attributes in
  key-value form}

  \item{vars}{keys to keep}
}
\value{
  data.frame with coordinates and attributes in key-value
  form
}
\description{
  Merge the coordinates and attributes of nodes or ways
}
\details{
  This function takes the tags dataframe, where the
  attributes dataframe is added. It then merges both
  dataframes by the id of the element. It also makes a
  subset, i.e. also the rows with key=vars will be kept. A
  geom column will be added. This can be one of line or
  point.
}
\author{
  chris
}

