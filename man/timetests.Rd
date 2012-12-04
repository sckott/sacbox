\name{timetests}
\alias{timetests}
\title{Get run times for all tests in a package.}
\usage{
  timetests(dir)
}
\arguments{
  \item{dir}{Path to the directory that contains the
  tests.}
}
\value{
  A data.frame sorted by elapsed run time for each test so
  that you can easily pick out the offending tests that are
  taking a long time.
}
\description{
  Get run times for all tests in a package.
}
\examples{
\dontrun{
timetests(dir = "/Users/ScottMac/github/rOpenSci/taxize_/inst/tests")
}
}

