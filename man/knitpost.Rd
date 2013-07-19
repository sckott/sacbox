\name{knitpost}
\alias{knitpost}
\title{Prepare a markdown post for jekyll.}
\usage{
  knitpost(input,
    output = "~/github/sac/schamberlain.github.com/_posts/",
    base.url = "/")
}
\arguments{
  \item{input}{The url for the markdown file, including the
  file name with .markdown or .mdown extension.}

  \item{base.url}{Leave as default.}
}
\description{
  Modified very slightly from
  http://jfisher-usgs.github.com/r/2012/07/03/knitr-jekyll/.
}
\examples{
\dontrun{
setwd("/Users/ScottMac/github/SChamberlain/schamberlain.github.com/_posts")
knitpost("/Users/ScottMac/github/SChamberlain/schamberlain.github.com/_drafts/2012-07-20-global-names-resolver.Rmd")
}
}

