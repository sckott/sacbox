\name{req}
\alias{req}
\title{Easy require/library.}
\usage{
  req(x)
}
\arguments{
  \item{x}{Vector of quoted package names to load.}
}
\description{
  Easy require/library.
}
\examples{
req(list("XML", "doMC", "plyr", "RCurl", "stringr"))
}

