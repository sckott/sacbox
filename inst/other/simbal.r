#' Simulate balanced and unbalanced trees.
#' 
#' @import ape apTreeshape plyr
#' @importFrom phytools fastBM
#' @param t Number of tips (i.e., species). Defaults to 10 tips.
#' @param metric Methods to use to generate trees, one of "colless", "beta", or  
#' 		gamma (see details). Defaults to "colless".
#' @param n Number of trees to produce. Defaults to 10 trees.
#' @param cutlow Value at which to filter trees on the low (e.g., unbalanced) side of the metric. 
#' @param cuthigh Value at which to filter trees on the high (e.g., balanced) side of the metric.
#' @return List of length n, each elemen of two parts (one is the balance 
#' 		metric, and the other is the phylogeny)
#' @details See the \code{apTreeshape} package for a description of the beta-splitting 
#' 		metric. Both Colless' metric and beta describe the extent to which a tree is 
#' 		balanced or not. 
#' @examples 
#' # Simulate 20 trees, each with 10 tips (=species), then pull out trees and metrics, 
#' # using the colless' statistic (fxn: colless) of the apTreeshape package
#' out <- simbal(t = 10, metric = "colless", n = 20, cutlow = -0.5, cuthigh = 0.5) # run it
#' out$bal # get the balanced trees
#' out$unbal # get the unbalanced trees
#' 
#' # Using beta-splitting (fxn: maxlik.betasplit) metric of the apTreeshape package
#' out <- simbal(t = 10, metric = "beta", n = 100, cutlow = -0.5, cuthigh = 0.5) # run it
#' out$bal # get the balanced trees
#' out$unbal # get the unbalanced trees
#' 
#' # Using gamma statistic (fxn: gammaStat) metric of the ape package
#' out <- simbal(t = 10, metric = "gamma", n = 100, cutlow = 1.6, cuthigh = 3) # run it
#' out$bal # get the trees with nodes close to root on average
#' out$unbal # get the trees with nodes close to the tips on average
#' @export
simbal <- function(t = 10, metric = "colless", n = 10, 
									 cutlow = -0.5, cuthigh = 0.5) 
{
	# Function to make trees and calculate beta-splitting metric
	beta <- function(t = t, n = n, cutlow = cutlow, cuthigh = cuthigh) {
		iter <- 0
		trees <- list()
		while( length(compact(trees)) < n ){
			iter <- iter + 1
			tree <- rcoal(t) # make a tree
			xx <- as.treeshape(tree) # convert to apTreeshape format
			c_ <- maxlik.betasplit(xx)[[1]] # calculate beta-splitting metric
			if(c_ < cutlow){ trees[[iter]] <- tree } else { trees[[iter]] <- NULL }
		}
		balanced <- compact(trees)
		
		iter2 <- 0
		trees2 <- list()
		while( length(compact(trees2)) < n ){
			iter2 <- iter2 + 1
			tree2 <- rcoal(t) # make a tree
			xx2 <- as.treeshape(tree2) # convert to apTreeshape format
			c_ <- maxlik.betasplit(xx2)[[1]] # calculate beta-splitting metric
			if(c_ > cuthigh){ trees2[[iter2]] <- tree2 } else { trees2[[iter2]] <- NULL }
		}
		unbalanced <- compact(trees2)
		
		list(bal = balanced, unbal = unbalanced)
	}
	
	# Function to make trees and calculate colless' metric
	colless_ <- function(t = t, n = n, cutlow = cutlow, cuthigh = cuthigh) {
		iter <- 0
		trees <- list()
		while( length(compact(trees)) < n ){
			iter <- iter + 1
			tree <- rcoal(t) # make a tree
			xx <- as.treeshape(tree) # convert to apTreeshape format
			c_ <- colless(xx, "yule") # calculate colless' metric
			if(c_ < cutlow){ trees[[iter]] <- tree } else { trees[[iter]] <- NULL }
		}
		balanced <- compact(trees)
		
		iter2 <- 0
		trees2 <- list()
		while( length(compact(trees2)) < n ){
			iter2 <- iter2 + 1
			tree2 <- rcoal(t) # make a tree
			xx2 <- as.treeshape(tree2) # convert to apTreeshape format
			c_ <- colless(xx2, "yule") # calculate colless' metric
			if(c_ > cuthigh){ trees2[[iter2]] <- tree2 } else { trees2[[iter2]] <- NULL }
		}
		unbalanced <- compact(trees2)
		
		list(bal = balanced, unbal = unbalanced)
	}
	
	# Function to make trees and calculate gammaStat
	gammastat <- function(t = t, n = n, cutlow = cutlow, cuthigh = cuthigh) {
		iter <- 0
		trees <- list()
		while( length(compact(trees)) < n ){
			iter <- iter + 1
			tree <- rcoal(t) # make a tree
			c_ <- gammaStat(tree) # calculate gamma statistic metric
			if(c_ > cuthigh){ trees[[iter]] <- tree } else { trees[[iter]] <- NULL }
		}
		neartips <- compact(trees)
		
		iter2 <- 0
		trees2 <- list()
		while( length(compact(trees2)) < n ){
			iter2 <- iter2 + 1
			tree2 <- rcoal(t) # make a tree
			c_ <- gammaStat(tree2) # calculate gamma statistic metric
			if(c_ < cutlow){ trees2[[iter2]] <- tree2 } else { trees2[[iter2]] <- NULL }
		}
		nearroot <- compact(trees2)
		
		list(bal = nearroot, unbal = neartips)
	}
	
	metric <- match.arg(metric, choices=c("colless","beta","gamma"), several.ok=F)
	
	if(metric == "beta"){
		betalist <- beta(t=t, n=n, cutlow=cutlow, cuthigh=cuthigh)
		collesslist <- NULL
		gammalist <- NULL
	} else
		if(metric == "colless"){
			betalist <- NULL
			collesslist <- colless_(t=t, n=n, cutlow=cutlow, cuthigh=cuthigh)
			gammalist <- NULL
		} else
			if(metric == "gamma"){
				betalist <- NULL
				collesslist <- NULL
				gammalist <- gammastat(t=t, n=n, cutlow=cutlow, cuthigh=cuthigh)
			} else
			stop("metric must be one of 'beta', 'colless', or 'gamma'")
	
	compact(list(betalist, collesslist, gammalist))[[1]]
}