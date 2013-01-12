#' Make a q-q plot for model diagnostics
#' 
#' @import ggplot2
#' @param model A model object, can be from lm() or glm() call I think. 
#' @examples
#' library(ggplot2)
#' data(mtcars)
#' mod <- lm(mpg ~ qsec, data=mtcars)
#' ggqqplot(mod)
#' @export
ggqqplot <- function(model)
{
	y <- quantile(model$resid[!is.na(model$resid)], c(0.25, 0.75))
	x <- qnorm(c(0.25, 0.75))
	slope <- diff(y)/diff(x)
	int <- y[1L] - slope * x[1L]
	p <- ggplot(model, aes(sample=.resid)) +
		theme_bw(base_size=18) +
		geom_point(stat = "qq", size=5) +
		geom_abline(slope = slope, intercept = int, color="blue")	
	return(p)
}