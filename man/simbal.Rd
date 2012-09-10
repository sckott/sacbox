\name{simbal}
\alias{simbal}
\title{Simulate balanced and unbalanced trees.}
\usage{
  simbal(t = 10, metric = "colless", n = 10, cutlow,
    cuthigh)
}
\arguments{
  \item{t}{Number of tips (i.e., species). Defaults to 10
  tips.}

  \item{metric}{Methods to use to generate trees, one of
  "colless", "beta", or gamma (see details). Defaults to
  "colless".}

  \item{n}{Number of trees to produce. Defaults to 10
  trees.}

  \item{cutlow}{Value at which to filter trees on the low
  (e.g., unbalanced) side of the metric.}

  \item{cuthigh}{Value at which to filter trees on the high
  (e.g., balanced) side of the metric.}
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
# Simulate 20 trees, each with 10 tips (=species), then pull out trees and metrics,
# using the colless' statistic (fxn: colless) of the apTreeshape package
out <- simbal(t = 10, metric = "colless", n = 20, cutlow = -0.5, cuthigh = 0.5) # run it
as.numeric(sapply(out, "[", "colstat")) # get the balance metric values
compact(lapply(out, function(x) x$colless_bal)) # get the balanced trees
compact(lapply(out, function(x) x$colless_unbal)) # get the unbalanced trees

# Using beta-splitting (fxn: maxlik.betasplit) metric of the apTreeshape package
out <- simbal(t = 10, metric = "beta", n = 20, cutlow = -0.5, cuthigh = 0.5) # run it
as.numeric(sapply(out, "[", "beta")) # get the balance metric values
compact(lapply(out, function(x) x$beta_bal)) # get the balanced trees
compact(lapply(out, function(x) x$beta_unbal)) # get the unbalanced trees

# Using gamma statistic (fxn: gammaStat) metric of the ape package
out <- simbal(t = 10, metric = "gamma", n = 1000, cutlow = 1, cuthigh = 3) # run it
as.numeric(sapply(out, "[", "gamstat")) # get the balance metric values
trees <- compact(lapply(out, function(x) x$gamma_neartips)) # get the balanced trees
trees2 <- compact(lapply(out, function(x) x$gamma_nearroot)) # get the unbalanced trees
}

