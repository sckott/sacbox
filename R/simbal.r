#' Simulate balanced and unbalanced trees.
#' 
#' @import ape phytools apTreeshape plyr
#' @param t Number of tips (i.e., species). Defaults to 10 tips.
#' @param metric Methods to use to generate trees, one of "colless" or "beta" (see details). 
#' 		Defaults to "colless".
#' @param n Number of trees to produce. Defaults to 10 trees.
#' @param cutlow Value at which to filter trees on the low (e.g., unbalanced) side of the metric. 
#' @param cuthigh Value at which to filter trees on the high (e.g., balanced) side of the metric.
#' @return List of length n, each elemen of two parts (one is the balance 
#' 		metric, and the other is the phylogeny)
#' @details See the \code{apTreeshape} package for a description of the beta-splitting 
#' 		metric. Both Colless' metric and beta describe the extent to which a tree is 
#' 		balanced or not. 
#' @examples 
#' # Simulate 20 trees, each with 10 tips (=species), then pull out trees and metrics
#' out <- simbal(t = 10, metric = "colless", n = 20, cutlow = -0.5, cuthigh = 0.5) # run it
#' as.numeric(sapply(out, "[", "colstat")) # get the balance metric values
#' compact(lapply(out, function(x) x$colless_bal)) # get the balanced trees
#' compact(lapply(out, function(x) x$colless_unbal)) # get the unbalanced trees
#' @export
simbal <- function(t = 10, metric = "colless", n = 10, cutlow, cuthigh) 
{
	trees <- replicate(n, rcoal(t), simplify=F) # make a tree with n species

	beta <- function(x) {
		t_bal <- NULL
		t_unbal <- NULL
		xx <- as.treeshape(x) # convert trees to aptreeshape format trees
		b <- maxlik.betasplit(xx)[[1]] # calculate beta for all trees
		if(b < cutlow){ t_bal <- I(x) } else
			if(b > cuthigh) { t_unbal <- I(x) } # returns tree if less than some level of balance	
				end
		compact(list(beta = b, beta_bal = t_bal, beta_unbal = t_unbal))
	}
	colless_ <- function(x) {
		t_bal <- NULL
		t_unbal <- NULL
		xx <- as.treeshape(x) # convert trees to aptreeshape format trees
		c_ <- colless(xx, "yule") # calculate colless' metric
		if(c_ < cutlow){ t_bal <- x } else
			if(c_ > cuthigh) { t_unbal <- x } # returns tree if less than some level of balance	
				end
		compact(list(colstat = c_, colless_bal = t_bal, colless_unbal = t_unbal))
	}
	
	if(metric == "beta"){
		betalist <- lapply(trees, beta)
		collesslist <- NULL
	} else
		if(metric == "colless"){
			betalist <- NULL
			collesslist <- lapply(trees, colless_)
		} else
			stop("metric must be one of 'beta' or 'colless'")
	
	compact(list(betalist, collesslist))[[1]]
}