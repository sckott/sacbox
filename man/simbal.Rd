\name{simbal}
\alias{simbal}
\title{Simulate balanced and unbalanced trees.}
\usage{
  simbal(t, metrics, n)
}
\arguments{
  \item{t}{Number of tips (i.e., species)}

  \item{metrics}{Methods to use to generate trees}

  \item{n}{Number of trees to produce}
}
\value{
  List of length n, of two element lists (element one is
  the balance metric, and the other is the phylogeny)
}
\description{
  Simulate balanced and unbalanced trees.
}
\examples{
simbal(t = 10, metrics = "beta", n = 20)
out <- simbal(t = 10, metrics = "colless", n = 20) # run it
as.numeric(sapply(out, "[", "colstat")) # get the balance metric values
compact(lapply(out, function(x) x$colless_bal)) # get the balanced trees
compact(lapply(out, function(x) x$colless_unbal)) # get the unbalanced trees
}

