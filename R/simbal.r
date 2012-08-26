#' Simulate balanced and unbalanced trees.
#' 
#' @import ape phytools apTreeshape plyr
#' @param t Number of tips (i.e., species)
#' @param metrics Methods to use to generate trees
#' @param n Number of trees to produce
#' @return List of length n, of two element lists (element one is the balance 
#' 		metric, and the other is the phylogeny)
#' @examples 
#' simbal(t = 10, metrics = "beta", n = 20)
#' out <- simbal(t = 10, metrics = "colless", n = 20) # run it
#' as.numeric(sapply(out, "[", "colstat")) # get the balance metric values
#' compact(lapply(out, function(x) x$colless_bal)) # get the balanced trees
#' compact(lapply(out, function(x) x$colless_unbal)) # get the unbalanced trees
#' @export
simbal <- function(t, metrics, n) {
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
		if(c_ < 1){ t_bal <- I(x) } else
			if(c_ > 1) { t_unbal <- I(x) } # returns tree if less than some level of balance	
		end
		compact(list(colstat = c_, colless_bal = t_bal, colless_unbal = t_unbal))
	}
	
	if(metrics == "beta"){
		betalist <- lapply(trees, beta)
		collesslist <- NULL
	} else
		if(metrics == "colless"){
			betalist <- NULL
			collesslist <- lapply(trees, colless_)
		} else
			stop("metrics must be one of 'beta' or 'colless'")
	
	compact(list(betalist, collesslist))[[1]]
}