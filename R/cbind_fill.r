#' cbind version of rbind.fill - force columns of data.frames together
#' 
#' @param ... Two or more data.frames or matrices
#' @param as_df Return as data.frame? default=FALSE
#' @examples
#' x <- data.frame(letters[1:5])
#' y <- data.frame(letters[1:10])
#' cbind_fill(x, y)
#' @export
cbind_fill <- function(..., as_df = FALSE)
{
	nm <- list(...) 
	nm <-lapply(nm, as.matrix)
	n <- max(sapply(nm, nrow)) 
	temp <- do.call(cbind, lapply(nm, function (x) 
		rbind(x, matrix(, n-nrow(x), ncol(x))))) 
	if(as_df){data.frame(temp)} else {temp}
}