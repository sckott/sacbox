#' Get run times for all tests in a package. 
#' 
#' @importFrom reshape sort_df
#' @importFrom plyr ldply
#' @param dir Path to the directory that contains the tests. 
#' @return A data.frame sorted by elapsed run time for each test so that you can 
#' 		easily pick out the offending tests that are taking a long time.
#' @examples \dontrun{
#' timetests(dir = "/Users/ScottMac/github/rOpenSci/taxize_/inst/tests")
#' }
#' @export 
timetests <- function(dir) {
	fnames <- dir(dir)
	fnamespaths <- paste0(dir, "/", fnames)
	times <- lapply(fnamespaths, function(x) system.time( test_file(x) ))
	names(times) <- fnames
	df <- ldply(times)
	sort_df(df, "elapsed")	
}