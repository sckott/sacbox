\name{simbal}
\alias{simbal}
\title{Simulate balanced and unbalanced trees.}
\usage{
  simbal(t, metric = "colless", n)
}
\arguments{
  \item{t}{Number of tips (i.e., species)}

  \item{metric}{Methods to use to generate trees, one of
  "colless" or "beta" (see details).}

  \item{n}{Number of trees to produce}
}
\value{
  List of length n, each elemen of two parts (one is the
  balance metric, and the other is the phylogeny)
}
\description{
  Simulate balanced and unbalanced trees.
}
\details{
  See the \code{apTreeshape} package for a description of
  the beta-splitting metric. Both Colless' metric and beta
  describe the extent to which a tree is balanced or not.
}
\examples{
# Simulate 20 trees, each with 10 tips (=species), then pull out trees and metrics
out <- simbal(t = 10, metric = "colless", n = 20) # run it
as.numeric(sapply(out, "[", "colstat")) # get the balance metric values
compact(lapply(out, function(x) x$colless_bal)) # get the balanced trees
compact(lapply(out, function(x) x$colless_unbal)) # get the unbalanced trees
}

