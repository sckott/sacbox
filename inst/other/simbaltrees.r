#' Simulate a set of balanced and unbalanced trees. 
#' 
#' Could use this in asking questions aobut how phylogenetic tree balance 
#'   	influences ____ (madlib it). 
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
#' temp <- simbaltrees(tips=10, metric="colless", numtrees=10, cutlow=-0.5, cuthigh=0.5)
#' unique(temp$model)
#' }
#' @export
simbaltrees <- function(tips = 10, metric, numtrees, cutlow, cuthigh) {
  
  trees_colless_plants <- simbal(t=tips, metric=metric, n=numtrees, cutlow = cutlow, cuthigh = cuthigh)
  trees_colless_plants_bal <- trees_colless_plants$bal # get the balanced trees
  trees_colless_plants_unbal <- trees_colless_plants$unbal # get the unbalanced trees
  
  ### animal trees
  trees_colless_anim <- simbal(t=tips, metric=metric, n=numtrees, cutlow = cutlow, cuthigh = cuthigh)
  trees_colless_anim_bal <- trees_colless_anim$bal # get the balanced trees
  trees_colless_anim_unbal <- trees_colless_anim$unbal # get the unbalanced trees
  
  ################## Simulate traits on each tree
  # Plants
  ## Brownian motion traits
  t1_col_plants_bal_bm <- lapply(trees_colless_plants_bal, fastBM, a = 10, bounds=c(0,100))
  t1_col_plants_unbal_bm <- lapply(trees_colless_plants_unbal, fastBM, a = 10, bounds=c(0,100))
  # 	t2_col_plants_bal_bm <- lapply(trees_colless_plants_bal, fastBM, a = 10, bounds=c(0,100))
  # 	t2_col_plants_unbal_bm <- lapply(trees_colless_plants_unbal, fastBM, a = 10, bounds=c(0,100))
  # 	abd_col_plants_bal_bm <- lapply(t2_col_plants_bal_bm, function(x) exp(x))
  # 	abd_col_plants_unbal_bm <- lapply(t2_col_plants_unbal_bm, function(x) exp(x))
  
  ## Random traits - randomize traits across tips - shuffles names, but doesn't move position in tree of course
  randtips <- function(w){
    names_ <- names(w)
    names_shuff <- sample(names_, size=length(names_), replace=F)
    names(w) <- names_shuff
    w
  }
  t1_col_plants_bal_rand <- lapply(t1_col_plants_bal_bm, randtips)
  t1_col_plants_unbal_rand <- lapply(t1_col_plants_unbal_bm, randtips)
  # 	abd_col_plants_bal_rand <- lapply(abd_col_plants_bal_bm, randtips)
  # 	abd_col_plants_unbal_rand <- lapply(abd_col_plants_unbal_bm, randtips)
  
  ## Orntsein-Uhlenbeck model for conservation of trait evolution - 1 optima
  t1_col_plants_bal_ou <- lapply(trees_colless_plants_bal, rTraitCont, model = "OU", sigma = 1, alpha=10, theta=1)
  t1_col_plants_unbal_ou <- lapply(trees_colless_plants_unbal, rTraitCont, model = "OU", sigma = 1, alpha=10, theta=1)
  # 	t2_col_plants_bal_ou <- lapply(trees_colless_plants_bal, rTraitCont, model = "OU", sigma = 1, alpha=10, theta=1)
  # 	t2_col_plants_unbal_ou <- lapply(trees_colless_plants_unbal, rTraitCont, model = "OU", sigma = 1, alpha=10, theta=1)
  # 	abd_col_plants_bal_ou <- lapply(t2_col_plants_bal_ou, function(x) exp(x))
  # 	abd_col_plants_unbal_ou <- lapply(t2_col_plants_unbal_ou, function(x) exp(x))
  
  # Animals
  ## Brownian motion traits
  t1_col_anim_bal_bm <- llply(trees_colless_anim_bal, fastBM, a = 10, bounds=c(0,100))
  t1_col_anim_unbal_bm <- llply(trees_colless_anim_unbal, fastBM, a = 10, bounds=c(0,100))
  # 	t2_col_anim_bal_bm <- llply(trees_colless_anim_bal, fastBM, a = 10, bounds=c(0,100))
  # 	t2_col_anim_unbal_bm <- llply(trees_colless_anim_unbal, fastBM, a = 10, bounds=c(0,100))
  # 	abd_col_anim_bal_bm <- lapply(t2_col_anim_bal_bm, function(x) exp(x))
  # 	abd_col_anim_unbal_bm <- lapply(t2_col_anim_unbal_bm, function(x) exp(x))
  
  ## Random traits - randomize traits across tips - shuffles names, but doesn't move position in tree of course
  t1_col_anim_bal_rand <- lapply(t1_col_anim_bal_bm, randtips)
  t1_col_anim_unbal_rand <- lapply(t1_col_anim_unbal_bm, randtips)
  # 	abd_col_anim_bal_rand <- lapply(abd_col_anim_bal_bm, randtips)
  # 	abd_col_anim_unbal_rand <- lapply(abd_col_anim_unbal_bm, randtips)
  
  ## Orntsein-Uhlenbeck model for conservation of trait evolution - 1 optima
  t1_col_anim_bal_ou <- lapply(trees_colless_anim_bal, rTraitCont, model = "OU", sigma = 1, alpha=10, theta=1)
  t1_col_anim_unbal_ou <- lapply(trees_colless_anim_unbal, rTraitCont, model = "OU", sigma = 1, alpha=10, theta=1)
  # 	t2_col_anim_bal_ou <- lapply(trees_colless_anim_bal, rTraitCont, model = "OU", sigma = 1, alpha=10, theta=1)
  # 	t2_col_anim_unbal_ou <- lapply(trees_colless_anim_unbal, rTraitCont, model = "OU", sigma = 1, alpha=10, theta=1)
  # 	abd_col_anim_bal_ou <- lapply(t2_col_anim_bal_ou, function(x) exp(x))
  # 	abd_col_anim_unbal_ou <- lapply(t2_col_anim_unbal_ou, function(x) exp(x))
  
  
  ################## Get plant-animal phylogenetic tree pairs
  # Balanced trees
  tree_pairs_bal <- list(trees_colless_plants_bal, trees_colless_anim_bal)
  
  # Unbalanced trees
  tree_pairs_unbal <- list(trees_colless_plants_unbal, trees_colless_anim_unbal)
 	
  # Pairs of traits from balanced trees
  ## Brownian motion traits
  all_t1_bal_bm <- list(t1_col_plants_bal_bm, t1_col_anim_bal_bm)
#   all_t1_bal_bm <- list(t1_col_plants_bal_bm[1:length(tree_pairs_bal[[1]])], 
#                         t1_col_anim_bal_bm[1:length(tree_pairs_bal[[1]])])
  # 	all_abd_bal_bm <- list(abd_col_plants_bal_bm[1:length(tree_pairs_bal[[1]])], 
  # 											abd_col_anim_bal_bm[1:length(tree_pairs_bal[[1]])])
  ## Random traits
  all_t1_bal_rand <- list(t1_col_plants_bal_rand, t1_col_anim_bal_rand)
#   all_t1_bal_rand <- list(t1_col_plants_bal_rand[1:length(tree_pairs_bal[[1]])], 
#                           t1_col_anim_bal_rand[1:length(tree_pairs_bal[[1]])])
  # 	all_abd_bal_rand <- list(abd_col_plants_bal_rand[1:length(tree_pairs_bal[[1]])], 
  # 											abd_col_anim_bal_rand[1:length(tree_pairs_bal[[1]])])
  ## OU traits
  all_t1_bal_ou <- list(t1_col_plants_bal_ou, t1_col_anim_bal_ou)
#   all_t1_bal_ou <- list(t1_col_plants_bal_ou[1:length(tree_pairs_bal[[1]])], 
#                         t1_col_anim_bal_ou[1:length(tree_pairs_bal[[1]])])
  # 	all_abd_bal_ou <- list(abd_col_plants_bal_ou[1:length(tree_pairs_bal[[1]])], 
  # 											abd_col_anim_bal_ou[1:length(tree_pairs_bal[[1]])])
  
  # Pairs of traits form unbalanced trees
  ## Brownian motion traits
  all_t1_unbal_bm <- list(t1_col_plants_unbal_bm, t1_col_anim_unbal_bm)
#   all_t1_unbal_bm <- list(t1_col_plants_unbal_bm[1:length(tree_pairs_unbal[[1]])], 
#                           t1_col_anim_unbal_bm[1:length(tree_pairs_unbal[[1]])])
  # 	all_abd_unbal_bm <- list(abd_col_plants_unbal_bm[1:length(tree_pairs_unbal[[1]])], 
  # 												abd_col_anim_unbal_bm[1:length(tree_pairs_unbal[[1]])])
  ## Random traits
  all_t1_unbal_rand <- list(t1_col_plants_unbal_rand, t1_col_anim_unbal_rand)
#   all_t1_unbal_rand <- list(t1_col_plants_unbal_rand[1:length(tree_pairs_unbal[[1]])], 
#                             t1_col_anim_unbal_rand[1:length(tree_pairs_unbal[[1]])])
  # 	all_abd_unbal_rand <- list(abd_col_plants_unbal_rand[1:length(tree_pairs_unbal[[1]])], 
  # 												abd_col_anim_unbal_rand[1:length(tree_pairs_unbal[[1]])])
  ## OU traits
  all_t1_unbal_ou <- list(t1_col_plants_unbal_ou, t1_col_anim_unbal_ou)
#   all_t1_unbal_ou <- list(t1_col_plants_unbal_ou[1:length(tree_pairs_unbal[[1]])], 
#                           t1_col_anim_unbal_ou[1:length(tree_pairs_unbal[[1]])])
  # 	all_abd_unbal_ou <- list(abd_col_plants_unbal_ou[1:length(tree_pairs_unbal[[1]])], 
  # 												abd_col_anim_unbal_ou[1:length(tree_pairs_unbal[[1]])])
  
  
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
#   listoftraitvecs <- all_t1_bal_ou
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
  ## BM
  mats_traits1_bal_bm_ratio <- sim_traits_nets(all_t1_bal_bm, method = "r", value = 1.5)
  mats_traits1_unbal_bm_ratio <- sim_traits_nets(all_t1_unbal_bm, method = "r", value = 1.5)
  # 	mats_traitsabd_bal_bm_ratio <- sim_traits_nets(all_abd_bal_bm, method = "r", value = 1.5)
  # 	mats_traitsabd_unbal_bm_ratio <- sim_traits_nets(all_abd_unbal_bm, method = "r", value = 1.5)
  ## Random
  mats_traits1_bal_rand_ratio <- sim_traits_nets(all_t1_bal_rand, method = "r", value = 1.5)
  mats_traits1_unbal_rand_ratio <- sim_traits_nets(all_t1_unbal_rand, method = "r", value = 1.5)
  # 	mats_traitsabd_bal_rand_ratio <- sim_traits_nets(all_abd_bal_rand, method = "r", value = 1.5)
  # 	mats_traitsabd_unbal_rand_ratio <- sim_traits_nets(all_abd_unbal_rand, method = "r", value = 1.5)
  ## OU
  mats_traits1_bal_ou_ratio <- sim_traits_nets(all_t1_bal_ou, method = "r", value = 1.5)
  mats_traits1_unbal_ou_ratio <- sim_traits_nets(all_t1_unbal_ou, method = "r", value = 1.5)
  # 	mats_traitsabd_bal_ou_ratio <- sim_traits_nets(all_abd_bal_ou, method = "r", value = 1.5)
  # 	mats_traitsabd_unbal_ou_ratio <- sim_traits_nets(all_abd_unbal_ou, method = "r", value = 1.5)
  
  # complementarity	
  ## BM
  mats_traits1_bal_bm_comp <- sim_traits_nets(all_t1_bal_bm, method = "c", value = 2)
  mats_traits1_unbal_bm_comp <- sim_traits_nets(all_t1_unbal_bm, method = "c", value = 2)
  # 	mats_traitsabd_bal_bm_comp <- sim_traits_nets(all_abd_bal_bm, method = "c", value = 2)
  # 	mats_traitsabd_unbal_bm_comp <- sim_traits_nets(all_abd_unbal_bm, method = "c", value = 2)
  ## Random
  mats_traits1_bal_rand_comp <- sim_traits_nets(all_t1_bal_rand, method = "c", value = 2)
  mats_traits1_unbal_rand_comp <- sim_traits_nets(all_t1_unbal_rand, method = "c", value = 2)
  # 	mats_traitsabd_bal_rand_comp <- sim_traits_nets(all_abd_bal_rand, method = "c", value = 2)
  # 	mats_traitsabd_unbal_rand_comp <- sim_traits_nets(all_abd_unbal_rand, method = "c", value = 2)
  ## OU
  mats_traits1_bal_ou_comp <- sim_traits_nets(all_t1_bal_ou, method = "c", value = 2)
  mats_traits1_unbal_ou_comp <- sim_traits_nets(all_t1_unbal_ou, method = "c", value = 2)
  # 	mats_traitsabd_bal_ou_comp <- sim_traits_nets(all_abd_bal_ou, method = "c", value = 2)
  # 	mats_traitsabd_unbal_ou_comp <- sim_traits_nets(all_abd_unbal_ou, method = "c", value = 2)
  
  # barrier (no need to give value parameter)
  ## BM
  mats_traits1_bal_bm_barr <- sim_traits_nets(all_t1_bal_bm, method = "b")
  mats_traits1_unbal_bm_barr <- sim_traits_nets(all_t1_unbal_bm, method = "b")
  # 	mats_traitsabd_bal_bm_barr <- sim_traits_nets(all_abd_bal_bm, method = "b")
  # 	mats_traitsabd_unbal_bm_barr <- sim_traits_nets(all_abd_unbal_bm, method = "b")
  ## Random
  mats_traits1_bal_rand_barr <- sim_traits_nets(all_t1_bal_rand, method = "b")
  mats_traits1_unbal_rand_barr <- sim_traits_nets(all_t1_unbal_rand, method = "b")
  # 	mats_traitsabd_bal_rand_barr <- sim_traits_nets(all_abd_bal_rand, method = "b")
  # 	mats_traitsabd_unbal_rand_barr <- sim_traits_nets(all_abd_unbal_rand, method = "b")
  ## OU
  mats_traits1_bal_ou_barr <- sim_traits_nets(all_t1_bal_ou, method = "b")
  mats_traits1_unbal_ou_barr <- sim_traits_nets(all_t1_unbal_ou, method = "b")
  # 	mats_traitsabd_bal_ou_barr <- sim_traits_nets(all_abd_bal_ou, method = "b")
  # 	mats_traitsabd_unbal_ou_barr <- sim_traits_nets(all_abd_unbal_ou, method = "b")
  
  
  ################## Calculate network metrics on matrices
  getnetmets <- function(balanced, unbalanced) {
    netmets_bal <- ldply(balanced, function(x) networklevel(x, 
        index = c("connectance", "links per species", "nestedness", "web asymmetry","togetherness")))
    netmets_unbal <- ldply(unbalanced, function(x) networklevel(x, 
        index = c("connectance", "links per species", "nestedness", "web asymmetry","togetherness")))
    data.frame( 
      type = c( rep("bal", length(balanced)), rep("unbal", length(unbalanced))), 
      rbind(netmets_bal, netmets_unbal) )
  }
  df_rand <- getnetmets(mats_rand_bal, mats_rand_unbal) # random networks
  
  ## BM
  df_traits1_bm_ratio <- getnetmets(mats_traits1_bal_bm_ratio, mats_traits1_unbal_bm_ratio) # ratio traits
  df_traits1_bm_comp <- getnetmets(mats_traits1_bal_bm_comp, mats_traits1_unbal_bm_comp) # complementarity traits
  df_traits1_bm_barr <- getnetmets(mats_traits1_bal_bm_barr, mats_traits1_unbal_bm_barr) # barrier traits
  
  ## Random
  df_traits1_rand_ratio <- getnetmets(mats_traits1_bal_rand_ratio, mats_traits1_unbal_rand_ratio) # ratio traits
  df_traits1_rand_comp <- getnetmets(mats_traits1_bal_rand_comp, mats_traits1_unbal_rand_comp) # complementarity traits
  df_traits1_rand_barr <- getnetmets(mats_traits1_bal_rand_barr, mats_traits1_unbal_rand_barr) # barrier traits
  
  ## OU
  df_traits1_ou_ratio <- getnetmets(mats_traits1_bal_ou_ratio, mats_traits1_unbal_ou_ratio) # ratio traits
  df_traits1_ou_comp <- getnetmets(mats_traits1_bal_ou_comp, mats_traits1_unbal_ou_comp) # complementarity traits
  df_traits1_ou_barr <- getnetmets(mats_traits1_bal_ou_barr, mats_traits1_unbal_ou_barr) # barrier traits
  
  alldat <- rbind(df_rand, 
                  df_traits1_bm_ratio, df_traits1_bm_comp, df_traits1_bm_barr,
                  df_traits1_rand_ratio, df_traits1_rand_comp, df_traits1_rand_barr,
                  df_traits1_ou_ratio, df_traits1_ou_comp, df_traits1_ou_barr)
  alldat$traitevol <- c( rep("Random",nrow(df_rand)), 
                     rep("BM",nrow(df_traits1_bm_ratio)),
                     rep("BM",nrow(df_traits1_bm_comp)),
                     rep("BM",nrow(df_traits1_bm_barr)),
                     rep("Random",nrow(df_traits1_rand_ratio)),
                     rep("Random",nrow(df_traits1_rand_comp)),
                     rep("Random",nrow(df_traits1_rand_barr)),
                     rep("OU",nrow(df_traits1_ou_ratio)),
                     rep("OU",nrow(df_traits1_ou_comp)),
                     rep("OU",nrow(df_traits1_ou_barr)))
  alldat$netmethod <- c( rep("Random",nrow(df_rand)), 
                         rep("Ratio",nrow(df_traits1_bm_ratio)),
                         rep("Complementarity",nrow(df_traits1_bm_comp)),
                         rep("Barrier",nrow(df_traits1_bm_barr)),
                         rep("Ratio",nrow(df_traits1_rand_ratio)),
                         rep("Complementarity",nrow(df_traits1_rand_comp)),
                         rep("Barrier",nrow(df_traits1_rand_barr)),
                         rep("Ratio",nrow(df_traits1_ou_ratio)),
                         rep("Complementarity",nrow(df_traits1_ou_comp)),
                         rep("Barrier",nrow(df_traits1_ou_barr)))
  alldat$numsp <- rep(tips, nrow(alldat))
  alldat
}