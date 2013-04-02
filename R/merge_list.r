#' Recursively merge data.frame's
#' 
#' @param dfs A list of data.frame's
#' @param ... Arguments passed onto merge
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @examples
#' mtcars$cars <- row.names(mtcars)
#' df1 <- mtcars[,c(1:2,12)]
#' df2 <- mtcars[,c(3:4,12)]
#' df3 <- mtcars[,c(5:6,12)]
#' merge_list(dfs=list(df1, df2, df3), by="cars")
#' @export 
merge_list <- function(dfs, ...)
{
	dfs1 <- dfs[[1]]
	dfs2 <- dfs[-1]
	for(i in 1:length(dfs2)){
		dfs1 <- merge(dfs1, dfs2[[i]], all = TRUE, sort = FALSE, ...)
	}
	return( dfs1 )
}