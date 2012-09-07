\name{capwords}
\alias{capwords}
\title{Capitalize the first letter of a character string.}
\usage{
  capwords(s, strict = FALSE)
}
\arguments{
  \item{s}{A character string}

  \item{strict}{Should the algorithm be strict about
  capitalizing. Defaults to FALSE.}
}
\description{
  Capitalize the first letter of a character string.
}
\examples{
capwords(c("using AIC for model selection"))
capwords(c("using AIC for model selection"), strict=TRUE)
}

