\name{vecrep}
\alias{vecrep}
\title{Replace many elements in a vector at the same time}
\usage{
  vecrep(vec, mapping)
}
\description{
  Replace many elements in a vector at the same time
}
\examples{
countries <- c('United States', 'Ecuador', 'Russia', 'Russia', 'Ecuador')
vecrep(vec=countries, mapping=list(c('Ecuador', 'Russia', 'United States'), c('ECU', 'RUS', 'USA')))
}

