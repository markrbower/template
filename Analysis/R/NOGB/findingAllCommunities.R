findingAllCommunities <- function( grph, algo ) {
  # Reduce the size of the graph by finding membership communities
  edges <- get.edge.attribute( grph, 'weight' )
  if ( algo=='walktrap' ) {
    comms <- cluster_walktrap( grph, weights = edges )
  } else if ( algo=='infomap' ) {
    comms <- cluster_infomap( grph, edges )
  } else if ( algo=='fast_greedy' ) {
    comms <- cluster_fast_greedy( grph, edges )
  } else {
    print( 'ERROR in findingCommunities: could not determine algorithm.')
    stop()
  }
  
  # Find the community of tDrop
  times <- names(membership(comms))
  groups <- unname(membership(comms))
  
  # Vectorize
  sun <- unname( unlist( groups ) )
  
  result <- data.frame( time=times, tempCMTY=as.numeric(sun) )
  
  return( result )
}
