\name{lakedata}
\alias{lakedata}
\title{Search for taxonomy data from Plantminer.com}
\usage{
  lakedata(lakenumber)
}
\arguments{
  \item{lakenumber}{Number of the Minnesota DNR lake.}
}
\value{
  data.frame of results.
}
\description{
  Search for taxonomy data from Plantminer.com
}
\examples{
\dontrun{
lakedata(lakenumber = "17005600")

lakenumber <- c("17005600","09000600","17004400","17008500","17001300","33002800")
out <- lapply(lakenumber, lakedata) # run all lake numbers
out <- out[!out \%in\% "no data table"] # remove lakes with no data
do.call(rbind, out)
}
}

