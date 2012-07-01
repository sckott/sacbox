#' Unnest a nested list.
#' 
#' @param x A nested list.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @return A list, with no nesting, hopefully.
#' @keywords list nested
#' @examples \dontrun{
#' # a nested list
#' mylist <- list(); mylist_ <- list()
#' for(i in 1:5) {
#'    for(j in 1:5) {
#'      mylist[[j]] <- i*j
#'    } 
#'    mylist_[[i]] <- mylist
#'  }
#' 
#' # Unnest the list
#' unnest(mylist_)[[1]]
#' }
#' @export
unnest <- function(x) {
  if(is.null(names(x))) {
    list(unname(unlist(x)))
  }
  else {
    c(list(all=unname(unlist(x))), do.call(c, lapply(x, unnest)))
  }
}
