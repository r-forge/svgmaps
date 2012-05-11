\name{add_interactivity}
\alias{add_interactivity}
\title{add-interactivity}
\usage{
  add_interactivity(gr, data)
}
\arguments{
  \item{gr}{A grob object}

  \item{data}{A data frame}
}
\value{
  A garnished grob
}
\description{
  Adds interactivity to a grob
}
\details{
  This is a generic function which takes a grob and a
  datasets and garnishes the grob with the data. There is
  tooltip, highlight and link implemented now.
}
\author{
  chris
}

