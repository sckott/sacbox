#' Coerces data.frame columns to the specified classes
#' 
#' @param d A data.frame.
#' @param colClasses A vector of column attributes, one of: 
#'    numeric, factor, character, etc.
#' @examples
#' dat <- data.frame(xvar = seq(1:10), yvar = rep(c("a","b"),5)) # make a data.frame
#' str(dat)
#' str(colClasses(dat, c("factor","factor")))
#' @export
colClasses <- function(d, colClasses) {
  colClasses <- rep(colClasses, len=length(d))
  d[] <- lapply(seq_along(d), function(i) switch(colClasses[i], 
      numeric=as.numeric(d[[i]]), 
      character=as.character(d[[i]]), 
      Date=as.Date(d[[i]], origin='1970-01-01'), 
      POSIXct=as.POSIXct(d[[i]], origin='1970-01-01'), 
      factor=as.factor(d[[i]]),
      as(d[[i]], colClasses[i]) ))
  d
}