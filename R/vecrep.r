#' Replace many elements in a vector at the same time
#' @examples
#' countries <- c('United States', 'Ecuador', 'Russia', 'Russia', 'Ecuador')
#' vecrep(vec=countries, mapping=list(c('Ecuador', 'Russia', 'United States'), c('ECU', 'RUS', 'USA')))
vecrep <- function (vec, mapping) 
{
  mapped <- data.frame(mapping)
  names(mapped) <- c("a","b")
  mapped$b[match(vec, mapped$a)]
}