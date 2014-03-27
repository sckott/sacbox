#' Load default parameter values of a function, list of functions or all functions
#' in a package.
#' 
#' @importFrom plyr l_ply
#' @param fxn Function object, not character name of function.
#' @param pkg Package name.
#' @param path Path to a function.
#' @param envir Environment to load default parameters in to. Default is .GlobalEnv
#' @export
#' @examples \dontrun{
#' # A single function
#' foo <- function(x=5, y=4) x+y
#' load_defaults(foo)
#' x; y
#' 
#' # Many functions
#' foo <- function(x=5, y=4) x+y
#' bar <- function(a="a", b="b") c(a,b)
#' load_defaults(fxn=c(foo, bar))
#' 
#' # Function path
#' load_defaults(path="~/github/ropensci/rnoaa/R/noaa.r")
#' }

load_defaults <- function(fxn, pkg=NULL, path=NULL, envir=.GlobalEnv)
{
  
  if(!is.null(pkg))
    require(pkg)
  if(!is.null(path)){
    tempenv <- new.env()
    source(path, local=tempenv)
    fxn <- get(ls(tempenv), envir=tempenv)
  }
  
  iter <- function(x){
    argslist <- as.list(x, all=TRUE)
    args <- argslist[-length(argslist)]
    for(i in seq_along(args)){
      assign(names(args[i]), args[[i]], envir=envir)
    }
  }
  
  if(length(fxn)==1){ iter(fxn) } else { l_ply(fxn, iter)  }
}