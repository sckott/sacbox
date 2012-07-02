\name{renamefiles}
\alias{renamefiles}
\title{Rename a bunch of files in a directory with the same regex procedure.}
\usage{
  renamefiles(dir, pattern, replacement)
}
\arguments{
  \item{dir}{Path to directory for which you want to change
  the file names.}

  \item{pattern}{Pattern in file name to replace.}

  \item{replacement}{Text to replace pattern with.}
}
\value{
  Prints "Operation complete" to console when done; look at
  files in directory 'dir' for changes.
}
\description{
  For example, a file name may have spaces, and you can
  replace them with "_".
}
\examples{
\dontrun{
renamefiles("~/test", " ", "_")
}
}
\author{
  Scott Chamberlain \email{myrmecocystus@gmail.com}
}
\keyword{file}
\keyword{rename}

