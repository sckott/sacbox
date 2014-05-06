\name{ggqqplot}
\alias{ggqqplot}
\title{Make a q-q plot for model diagnostics}
\usage{
ggqqplot(model)
}
\arguments{
  \item{model}{A model object, can be from lm() or glm()
  call I think.}
}
\description{
Make a q-q plot for model diagnostics
}
\examples{
library(ggplot2)
data(mtcars)
mod <- lm(mpg ~ qsec, data=mtcars)
ggqqplot(mod)
}

