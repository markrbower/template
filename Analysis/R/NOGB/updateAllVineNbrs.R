updateAllVineNbrs <- function( grph, allMemberships ) {

  # Update all members.
  for ( t in allMemberships$time ) {
    grph <- set.vertex.attribute( grph, "vine", IDv( grph, t ), allMemberships[ allMemberships$time==t, 'final' ] )
  }
  
  return( grph )  
}
