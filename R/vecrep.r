#' Replace many elements in a vector at the same time
#' @param vec A vector
#' @param from From what
#' @param to To what
#' @export
#' @examples
#' countries <- c('United States', 'Ecuador', 'Russia', 'Russia', 'Ecuador')
#' vecrep(vec=countries, from=c('Ecuador', 'Russia', 'United States'), to=c('ECU', 'RUS', 'USA'))
vecrep <- function (vec, from, to) 
{ 
  to[match(vec, from)]
}