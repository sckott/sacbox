#' Capitalize the first letter of a character string.
#' 
#' @param s A character string
#' @param strict Should the algorithm be strict about capitalizing. Defaults to FALSE.
#' @examples 
#' capwords(c("using AIC for model selection"))
#' capwords(c("using AIC for model selection"), strict=TRUE)
capwords <- function(s, strict = FALSE) {
	cap <- function(s) paste(toupper(substring(s,1,1)),
		{s <- substring(s,2); if(strict) tolower(s) else s}, sep = "", collapse = " " )
		sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}