\name{vecrep}
\alias{vecrep}
\title{Replace many elements in a vector at the same time}
\usage{
  vecrep(vec, from, to)
}
\arguments{
  \item{vec}{A vector}

  \item{from}{From what}

  \item{to}{To what}
}
\description{
  Replace many elements in a vector at the same time
}
\examples{
countries <- c('United States', 'Ecuador', 'Russia', 'Russia', 'Ecuador')
vecrep(vec=countries, from=c('Ecuador', 'Russia', 'United States'), to=c('ECU', 'RUS', 'USA'))
}

