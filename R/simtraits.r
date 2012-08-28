#' Simulate traits on each tree.
#' 
#' @import plyr phytools
#' @param trees List of phylogenetic trees in "phylo" format. 
#' @param numtraits Number of traits to create.
#' @param method Method to simulate traits on a phylogeny, one of fastBM, etc. 
#' 		Only one method allowed.
#' @details You can specify how many traits you would like to create. One trait output
#' 		will always be abundance ('abd'). 
#' @return List of lists, each of which is a nanmed trait for each species in the phylogeny.
#' @examples
#' trees <- replicate(10, rcoal(10), simplify=F) # simulate trees
#' simtraits(trees = trees, numtraits = 2, method = "fastBM")
#' @export
## ab: abundance for each tip assigned from a lognormal distribution
## tr1: e.g., body size 
## tr2: trait that prvents ineractions with some species, e.g., tongue length  
simtraits <- function (trees, numtraits, method) 
{ 
  trait1 <- llply(trees, method)
  trait2 <- llply(trees, method)
  
  replicate( 2, lapply(trees, method) , simplify = FALSE )
 	
  abd_col_bal <- round(rlnorm(length(trees), meanlog=5, 1), 0)
  abd_col_unbal <- round(rlnorm(length(trees), meanlog=5, 1), 0)
}