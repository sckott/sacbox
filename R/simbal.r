#' Simulate balanced and unbalanced trees.
#' 
#' @import ape phytools apTreeshape plyr
#' @param t Number of tips (i.e., species)
#' @param metric Methods to use to generate trees, one of "colless" or "beta" (see details).
#' @param n Number of trees to produce
#' @return List of length n, each elemen of two parts (one is the balance 
#' 		metric, and the other is the phylogeny)
#' @details See the \code{apTreeshape} package for a description of the beta-splitting 
#' 		metric. Both Colless' metric and beta describe the extent to which a tree is 
#' 		balanced or not. 
#' @examples 
#' # Simulate 20 trees, each with 10 tips (=species), then pull out trees and metrics
#' out <- simbal(t = 10, metric = "colless", n = 20) # run it
#' as.numeric(sapply(out, "[", "colstat")) # get the balance metric values
#' compact(lapply(out, function(x) x$colless_bal)) # get the balanced trees
#' compact(lapply(out, function(x) x$colless_unbal)) # get the unbalanced trees
#' @export
simbal <- function(t, metric = "colless", n) 
{
	trees <- replicate(n, rcoal(t), simplify=F) # make a tree with n species

	beta <- function(x) {
		t_bal <- NULL
		t_unbal <- NULL
		xx <- as.treeshape(x) # convert trees to aptreeshape format trees
		b <- maxlik.betasplit(xx)[[1]] # calculate beta for all trees
		if(b < 1){ t_bal <- I(x) } else
			if(b > 1) { t_unbal <- I(x) } # returns tree if less than some level of balance	
				end
		compact(list(beta = b, beta_bal = t_bal, beta_unbal = t_unbal))
	}
	colless_ <- function(x) {
		t_bal <- NULL
		t_unbal <- NULL
		xx <- as.treeshape(x) # convert trees to aptreeshape format trees
		c_ <- colless(xx, "yule") # calculate colless' metric
		if(c_ < 0){ t_bal <- I(x) } else
			if(c_ > 0) { t_unbal <- I(x) } # returns tree if less than some level of balance	
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