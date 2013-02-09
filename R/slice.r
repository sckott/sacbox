#' Function to break up a vector by certain N sized chunks.
#' 
#' @import plyr
#' @param input Input character vector
#' @param by Number by which to split the input character vector
#' @export 
slice <- function(input, by = 2, equalpiecesof = NULL) {
	if(is.null(equalpiecesof)){	
		starts <- seq(1, length(input), by)
		tt <- lapply(starts, function(y) input[y:(y + (by - 1))])
		llply(tt, function(x) x[!is.na(x)])
	} else
	{
		splitby <- round(length(input)/equalpiecesof)+1
		starts <- seq(1, length(input), splitby)
		tt <- lapply(starts, function(y) input[y:(y + (splitby - 1))])
		llply(tt, function(x) x[!is.na(x)])
	}
}