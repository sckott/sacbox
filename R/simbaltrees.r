#' Simulate a set of balanced and unbalanced trees. 
#' 
#' Could use this in asking questions aobut how phylogenetic tree balance 
#' 		influences ____ (madlib it). 
#' 		
#' @import plyr ape apTreeshape bipartite ggplot2 reshape2
#' @importFrom phytools fastBM
#' @param tips 
#' @param metric Methods to use to generate trees, one of "colless", "beta", or  
#' 		gamma (see details). Defaults to "colless".
#' @param numtrees Number of trees to produce. Defaults to 10 trees.
#' @param cutlow Value at which to filter trees on the low (e.g., unbalanced) side of the metric.
#' @param cuthigh Value at which to filter trees on the high (e.g., balanced) side of the metric.
#' @examples \dontrun{
#' simbaltrees(tips=10, metric="beta", numtrees=10, cutlow=-0.5, cuthigh=0.5)
#' }
#' @export
simbaltrees <- function(tips, metric, numtrees, cutlow, cuthigh) {
	
	trees_colless_plants <- simbal(t=tips, metric=metric, n=numtrees, cutlow = cutlow, cuthigh = cuthigh)
	trees_colless_plants_bal <- out$bal # get the balanced trees
	trees_colless_plants_unbal <- out$unbal # get the unbalanced trees
	
	### animal trees
	trees_colless_anim <- simbal(t=tips, metric=metric, n=numtrees, cutlow = cutlow, cuthigh = cuthigh)
	trees_colless_anim_bal <- out$bal # get the balanced trees
	trees_colless_anim_unbal <- out$unbal # get the unbalanced trees
	
	################## Simulate traits on each tree
	# Plants
	t1_col_plants_bal <- llply(trees_colless_plants_bal, fastBM, a = 10, bounds=c(0,100))
	t1_col_plants_unbal <- llply(trees_colless_plants_unbal, fastBM, a = 10, bounds=c(0,100))
	t2_col_plants_bal <- llply(trees_colless_plants_bal, fastBM, a = 10, bounds=c(0,100))
	t2_col_plants_unbal <- llply(trees_colless_plants_unbal, fastBM, a = 10, bounds=c(0,100))
	abd_col_plants_bal <- round(rlnorm(length(trees_colless_plants_bal), meanlog=5, 1), 0)
	abd_col_plants_unbal <- round(rlnorm(length(trees_colless_plants_unbal), meanlog=5, 1), 0)
	
	# Animals
	t1_col_anim_bal <- llply(trees_colless_anim_bal, fastBM, a = 10, bounds=c(0,100))
	t1_col_anim_unbal <- llply(trees_colless_anim_unbal, fastBM, a = 10, bounds=c(0,100))
	t2_col_anim_bal <- llply(trees_colless_anim_bal, fastBM, a = 10, bounds=c(0,100))
	t2_col_anim_unbal <- llply(trees_colless_anim_unbal, fastBM, a = 10, bounds=c(0,100))
	abd_col_anim_bal <- round(rlnorm(length(trees_colless_anim_bal), meanlog=5, 1), 0)
	abd_col_anim_unbal <- round(rlnorm(length(trees_colless_anim_unbal), meanlog=5, 1), 0)
	
	
	################## Get plant-animal phylogenetic tree pairs
	# Balanced trees
	all_bal <- list(trees_colless_plants_bal, trees_colless_anim_bal)
	smaller_bal <- which.min(sapply(all_bal, length))
	larger_bal <- which.max(sapply(all_bal, length))
	tree_pairs_bal <- list(all_bal[[smaller_bal]], all_bal[[larger_bal]][1:length(all_bal[[smaller_bal]])])
	
	# Unbalanced trees
	all_unbal <- list(trees_colless_plants_unbal, trees_colless_anim_unbal)
	smaller_unbal <- which.min(sapply(all_unbal, length))
	larger_unbal <- which.max(sapply(all_unbal, length))
	tree_pairs_unbal <- list(all_unbal[[smaller_unbal]], all_unbal[[larger_unbal]][1:length(all_unbal[[smaller_unbal]])])
	
	# Pairs of traits from balanced trees
	all_t1_bal <- list(t1_col_plants_bal[1:length(tree_pairs_bal[[1]])], 
										 t1_col_anim_bal[1:length(tree_pairs_bal[[1]])])
	all_t2_bal <- list(t2_col_plants_bal[1:length(tree_pairs_bal[[1]])], 
										 t2_col_anim_bal[1:length(tree_pairs_bal[[1]])])
	all_abd_bal <- list(abd_col_plants_bal[1:length(tree_pairs_bal[[1]])], 
											abd_col_anim_bal[1:length(tree_pairs_bal[[1]])])
	
	# Pairs of traits form unbalanced trees
	all_t1_unbal <- list(t1_col_plants_unbal[1:length(tree_pairs_unbal[[1]])], 
											 t1_col_anim_unbal[1:length(tree_pairs_unbal[[1]])])
	all_t2_unbal <- list(t2_col_plants_unbal[1:length(tree_pairs_unbal[[1]])], 
											 t2_col_anim_unbal[1:length(tree_pairs_unbal[[1]])])
	all_abd_unbal <- list(abd_col_plants_unbal[1:length(tree_pairs_unbal[[1]])], 
												abd_col_anim_unbal[1:length(tree_pairs_unbal[[1]])])
	
	################## Simulate networks on each tree
	# Simulate completely random networks, regardless of traits, etc.
	sim_rand_nets <- function(listoftrees) {
		mats <- list()
		for(i in 1:length(listoftrees[[1]])) {
			m <- Ntip(listoftrees[[1]][[i]]) # number of plant species
			n <- Ntip(listoftrees[[2]][[i]]) # number of animal species
			# make random matrix and put matrix into list
			mm <- matrix(rbinom(m * n, 1, .5), ncol = m, nrow = n)  
			dimnames(mm)[[1]] <- as.list(listoftrees[[1]][[i]]$tip.label)
			dimnames(mm)[[2]] <- as.list(listoftrees[[2]][[i]]$tip.label)
			mats[[i]] <- mm
		}
		mats
	}
	mats_rand_bal <- sim_rand_nets(tree_pairs_bal)
	mats_rand_unbal <- sim_rand_nets(tree_pairs_unbal)
	
	# Simulate networks, with interactions propoprtional to trait matching
	sim_traits_nets <- function(listoftraitvecs, method = c("ratio","complementarity","barrier"), value) {
		mats <- list()
		method <- match.arg(method, c("ratio","complementarity","barrier"))
		message(paste("Using the ", method, " method"))
		for(i in 1:length(listoftraitvecs[[1]])) {
			# where the interaction occurs or not
			## Ratio - e.g., body size ratio, for gape limitation
			if(method == "ratio"){
				mm <- outer(listoftraitvecs[[1]][[i]], listoftraitvecs[[2]][[i]], 
										function(x,y) as.numeric(exp(x-y) < value)) 
			} else
				if(method == "complementarity"){
					mm <- outer(listoftraitvecs[[1]][[i]], listoftraitvecs[[2]][[i]], 
											function(x,y) as.numeric(abs(x-y) < value))
				}  else
					if(method == "barrier"){
						mm <- outer(listoftraitvecs[[1]][[i]], listoftraitvecs[[2]][[i]], 
												function(x,y) as.numeric(x > y))
					} else 
						stop("must be one of ratio, complementarity or barrier")
			dimnames(mm)[[1]] <- names(listoftraitvecs[[1]][[i]])
			dimnames(mm)[[2]] <- names(listoftraitvecs[[2]][[i]])
			if(sum(mm) == 0) { mm <- NULL } else 
				if( sum(mm) == nrow(mm) * ncol(mm) ) {mm <- NULL } else
				{ mm <- mm }
			mats[[i]] <- mm
		}
		mats[!sapply(mats, is.null)]
	}
	# ratio
	mats_traits1_bal_ratio <- sim_traits_nets(all_t1_bal, method = "r", value = 1.5)
	mats_traits1_unbal_ratio <- sim_traits_nets(all_t1_unbal, method = "r", value = 1.5)
	mats_traits2_bal_ratio <- sim_traits_nets(all_t2_bal, method = "r", value = 1.5)
	mats_traits2_unbal_ratio <- sim_traits_nets(all_t2_unbal, method = "r", value = 1.5)
	mats_traitsabd_bal_ratio <- sim_traits_nets(all_abd_bal, method = "r", value = 1.5)
	mats_traitsabd_unbal_ratio <- sim_traits_nets(all_abd_unbal, method = "r", value = 1.5)
	
	# complementarity
	mats_traits1_bal_comp <- sim_traits_nets(all_t1_bal, method = "c", value = 2)
	mats_traits1_unbal_comp <- sim_traits_nets(all_t1_unbal, method = "c", value = 2)
	mats_traits2_bal_comp <- sim_traits_nets(all_t2_bal, method = "c", value = 2)
	mats_traits2_unbal_comp <- sim_traits_nets(all_t2_unbal, method = "c", value = 2)
	mats_traitsabd_bal_comp <- sim_traits_nets(all_abd_bal, method = "c", value = 2)
	mats_traitsabd_unbal_comp <- sim_traits_nets(all_abd_unbal, method = "c", value = 2)
	
	# barrier (no need to give value parameter)
	mats_traits1_bal_barr <- sim_traits_nets(all_t1_bal, method = 'b')
	mats_traits1_unbal_barr <- sim_traits_nets(all_t1_unbal, method = 'b')
	mats_traits2_bal_barr <- sim_traits_nets(all_t2_bal, method = 'b')
	mats_traits2_unbal_barr <- sim_traits_nets(all_t2_unbal, method = 'b')
	mats_traitsabd_bal_barr <- sim_traits_nets(all_abd_bal, method = 'b')
	mats_traitsabd_unbal_barr <- sim_traits_nets(all_abd_unbal, method = 'b')
	
	
	################## Calculate network metrics on matrices
	library(bipartite)
	getnetmets <- function(balanced, unbalanced) {
		netmets_bal <- ldply(balanced, function(x) networklevel(x, 
																														index = c("connectance", "links per species", "nestedness")))
		netmets_unbal <- ldply(unbalanced, function(x) networklevel(x, 
																																index = c("connectance", "links per species", "nestedness")))
		data.frame( 
			type = c( rep("bal", length(balanced)), rep("unbal", length(unbalanced))), 
			rbind(netmets_bal, netmets_unbal) )
	}
	df_rand <- getnetmets(mats_rand_bal, mats_rand_unbal) # random networks
	
	# df_traitsabd_ratio <- getnetmets(mats_traitsabd_bal_ratio, mats_traitsabd_unbal_ratio) # neutral
	# df_traitsabd_comp <- getnetmets(mats_traitsabd_bal_comp, mats_traitsabd_unbal_comp) # neutral
	# df_traitsabd_barr <- getnetmets(mats_traitsabd_bal_barr, mats_traitsabd_unbal_barr) # neutral
	
	df_traits1_ratio <- getnetmets(mats_traits1_bal_ratio, mats_traits1_unbal_ratio) # ratio traits
	df_traits1_comp <- getnetmets(mats_traits1_bal_comp, mats_traits1_unbal_comp) # complementarity traits
	df_traits1_barr <- getnetmets(mats_traits1_bal_barr, mats_traits1_unbal_barr) # barrier traits
	
	df_traits2_ratio <- getnetmets(mats_traits2_bal_ratio, mats_traits2_unbal_ratio) # ratio traits
	df_traits2_comp <- getnetmets(mats_traits2_bal_comp, mats_traits2_unbal_comp) # complementarity traits
	df_traits2_barr <- getnetmets(mats_traits2_bal_barr, mats_traits2_unbal_barr) # barrier traits
	
	alldat <- rbind(df_rand, df_traits1_ratio, df_traits2_ratio,
									df_traits1_comp, df_traits2_comp,
									df_traits1_barr, df_traits2_barr)
	alldat$model <- c( rep("Random",nrow(df_rand)), 
										 rep("Ratio 1",nrow(df_traits1_ratio)),
										 rep("Ratio 2",nrow(df_traits2_ratio)),
										 rep("Complementarity 1",nrow(df_traits1_comp)),
										 rep("Complementarity 2",nrow(df_traits2_comp)),
										 rep("Barrier 1",nrow(df_traits1_barr)),
										 rep("Barrier 2",nrow(df_traits2_barr))
	)
	alldat$numsp <- rep(tips, nrow(alldat))
	alldat
}