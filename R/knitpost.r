#' Prepare a markdown post for jekyll. 
#' 
#' Modified very slightly from http://jfisher-usgs.github.com/r/2012/07/03/knitr-jekyll/.
#'
#' @import knitr
#' @param input The url for the markdown file, including the file name with .markdown or .mdown extension.
#' @param base.url Leave as default.
#' @examples \dontrun{ 
#' setwd("/Users/ScottMac/github/SChamberlain/schamberlain.github.com/_posts")
#' knitpost("/Users/ScottMac/github/SChamberlain/schamberlain.github.com/_drafts/2012-07-20-global-names-resolver.Rmd")
#' }
#' @export
knitpost <- function(input, output="~/github/sac/schamberlain.github.com/_posts/", base.url = "/") 
{
	opts_knit$set(base.url = base.url)
	fig.path <- paste0("img/", sub(".Rmd$", "", basename(input)), "/")
	opts_chunk$set(fig.path = fig.path)
	opts_chunk$set(fig.cap = "center")
	render_jekyll()
  fname <- paste0('~/github/sac/schamberlain.github.com/_drafts/',input)
	knit(fname, 
       output = paste0(output,sub('Rmd','md',input)), 
       envir = parent.frame())
# 	system(paste0("sed -i.bak '1,2d' ", fname, " > ", fname))
# 	system(paste0("sed -i.bak '1d' ", paste0(output,sub('Rmd','md',input))))
}