#' Easy require/library.
#' 
#' @param x Vector of quoted package names to load. 
#' @examples
#' req(list("XML", "doMC", "plyr", "RCurl", "stringr"))
#' @export
req <- function(x) {
  lapply(x, FUN = function(X) {
    do.call("require", list(X)) 
  })
}