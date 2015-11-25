mergeEdgeToGraph <- function( grph, tA, tB, cc, ed ) {
  library(igraph)
  
  # Added tB to args. Needs to replace whatever is here that finds the second node ...
  
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/mergeEdge.R')
  
#  (g,from,to,weight,defaults)
# Limit edges into each node
  
  edges <- E(grph)[from(IDv(grph,tB))]
  
  if ( length(edges) >= 5 ) {
    # Determine whether the new edge weight is larger than the smallest current weight
    weights <- get.edge.attribute(grph,'weight',edges)
    # If so, drop the smallest.
    if ( min(weights) < cc ) {
      idx <- which( weights==min(weights) )
      grph <- delete.edges( grph, edges[idx] )
    }  
  }
  if ( length(edges) < 5 ) {
    grph <- mergeEdge( grph, format(tA,scientific=FALSE), format(tB,scientific=FALSE), cc, list("ed",ed,"vine",-1) )
  }
  
  return( grph )
}
