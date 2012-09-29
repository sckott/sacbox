#' Search for taxonomy data from Plantminer.com
#' 
#' @import RCurl XML
#' @param lakenumber Number of the Minnesota DNR lake.
#' @return data.frame of results.
#' @examples \dontrun{
#' lakedata(lakenumber = "17005600")
#' 
#' lakenumber <- c("17005600","09000600","17004400","17008500","17001300","33002800")
#' out <- lapply(lakenumber, lakedata) # run all lake numbers
#' out <- out[!out %in% "no data table"] # remove lakes with no data
#' do.call(rbind, out)
#' }
#' @export
lakedata <- function(lakenumber)
{
	args <- list(downum = as.character(lakenumber))
	tt <- getForm(
		"http://www.dnr.state.mn.us/lakefind/showreport.html", 
		.params=args)
	if(!grepl("Number of fish per net", tt)){"no data table"} else
	{
		tables <- readHTMLTable(strsplit(tt, "\r\n\r\n\n\n")[[1]][[2]])
		names(tables) <- NULL
		nrows_ <- sapply(tables, nrow)
		nrows_[nrows_ == "NULL"] <- 0
		nrows_ <- as.list(nrows_)
		nrows_ <- do.call(c, nrows_)
		df <- tables[nrows_ > 4][[1]]
		names(df) <- 
			c("species","gear_used","caught","normal_range","avg_wgt_lb","normal_range_lbs")
		df$lake <- rep(lakenumber, nrow(df))
		df
	}
}