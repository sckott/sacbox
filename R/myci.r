#' Calculates the confidence interval of a vector of data.
#' 
#' @param vec A vector of numeric values.
#' @param ci The confidence interval to be calculated.
#' @return 
#' \item{upper}{Upper bound of interval.}
#' \item{mean}{Mean of data.}
#' \item{lower}{Lower bound of interval.}
#' 
#' @examples 
#' myci(rnorm(100))
#' @export
myci <- function(vec, ci = 0.95){
	a <- mean(vec)
	s <- sd(vec)
	n <- length(vec)
	error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
	return(c(upper = a + error, mean = a, lower = a - error))
}