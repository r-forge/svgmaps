\name{is_svgmap}
\alias{is_svgmap}
\title{Check svgmap data frame}
\usage{
  is_svgmap(df)
}
\arguments{
  \item{df}{A data frame}
}
\value{
  logical; TRUE if object is a valid svgmap_df
}
\description{
  Checks if the object is in valid svgmap data frame format
  This function takes a data frame and checks if it is a
  valid svgmap_df object. This means it has to be a data
  frame with columns named lon, lat, element_id, point_id,
  geom, order and an arbitrary number of additional
  variables. In addition it has to have the class svgmap_df
}

