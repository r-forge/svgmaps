\name{theme_map}
\alias{theme_map}
\title{A special theme for maps}
\usage{
  theme_map(base_size = 11, base_family = "")
}
\arguments{
  \item{base_size}{A number defining the size of the text}

  \item{base_family}{The text family}
}
\value{
  An ggplot options object
}
\description{
  This is a special theme for maps.
}
\details{
  This theme can be added to a svgmaps graphics with the
  '+' operator. It is very minimal. It supresses the axis
  and the ticks. The code is a contribution from Osmo
  Salomaa.
}
\examples{
data (muc)
svgmap(muc) + igeom_polygon() + theme_map()
}

