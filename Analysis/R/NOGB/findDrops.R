findDrops <- function( grph, window ) {
  # Find elements in the graph that are out of the correlation-time of the most recent entries.
  # Window size
  library( igraph )
  
  # Latest time in graph
  t <- sort( as.numeric( names( V(grph) ) ) )
  
#  dropWindow <- t[length(t)] - 2*timeWindow['usWindow']
  dropWindow <- t[length(t)] - window
  
  dropTimes <- which( t < dropWindow )  
    
  return( dropTimes )
}
