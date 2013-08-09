#' Rename a bunch of files in a directory with the same regex procedure.
#' 
#' For example, a file name may have spaces, and you can replace them with "_".
#' 
#' @importFrom plyr l_ply
#' @param dir Path to directory for which you want to change the file names.
#' @param pattern Pattern in file name to replace.
#' @param replacement Text to replace pattern with.
#' @return Prints "Operation complete" to console when done; look at files 
#'    in directory 'dir' for changes.
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @keywords file rename
#' @examples \dontrun{
#' renamefiles("~/test", " ", "_")
#' }
#' @export
renamefiles <- function(dir, pattern, replacement){
  
  setwd(dir)
  
  rename_ <- function(x) {
    file.rename(x, gsub(pattern, replacement, x))
  }
  
  files <- dir(full.names=T)
  l_ply(files, rename_)
  message("Operation complete") 
  
}