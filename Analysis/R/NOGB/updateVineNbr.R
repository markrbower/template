updateVineNbr <- function( grph, communityMembers, update ) {
  # Find the most common vine number in that community.
  vineNbr <- -1
  counts <- vector( mode='integer', length=10000 )
  for ( m in communityMembers ) {
    thisNbr <- unlist( get.vertex.attribute( grph, "vine", IDv( grph, m ) ) )
    if ( thisNbr > -1 ) {
      counts[thisNbr] <- counts[thisNbr] + 1
    }
#    if ( vineNbr > -1 ) {
#      if ( thisNbr > -1 & thisNbr < vineNbr ) {
#        vineNbr <- thisNbr
#      }
#    } else {
#      vineNbr <- thisNbr
#    }
  }
  if ( max(counts) > 0 ) {
    vineNbr <- which( counts == max(counts) )
  } else {
    vineNbr <- update$vineCount # increment this count in 'checkDatabaseUpdate.R'
  }
  
  # Update all members.
  for ( m in communityMembers ) {
    grph <- set.vertex.attribute( grph, "vine", IDv( grph, m ), vineNbr )
  }
  
  return( grph )  
}
