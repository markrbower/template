pluralities <- function( grph, tempMemberships ) {
  # For each number in tempMemberships, count the number of each existing group membership.
  # Find the winner for each.
  masterID <- attr( grph, 'masterID' )
  
  existing <- get.vertex.attribute( grph, 'vine' )
  
  tempNumbers <- sort(unique(tempMemberships$tempCMTY))
  tempMemberships <- cbind( tempMemberships, final=0 )
  
  assigned <- list()
  replaceIdx <- -1
  for ( tn in tempNumbers ) {
    counts <- table( unlist( existing[which(tempMemberships$tempCMTY==tn)] ) )
    
    # What if counts are equal for two or more categories?
    # Take "the first'; the one with the lowest timestamp.
    maxID <- as.numeric(names(which(counts == max(counts))))
    if ( maxID[1] %in% assigned ) {
      # Duplicate
      replaceIdx <- replaceIdx - 1
      tempMemberships[tempMemberships$tempCMTY==tn,'final'] <- replaceIdx # a unique negative number
    } else {
      assigned <- union( assigned, maxID[1] )
      tempMemberships[tempMemberships$tempCMTY==tn,'final'] <- maxID[1]
    }  
  }
  
  # Check for ID<0. Use masterID to determine new increment.
  neednewid_n <- which( tempMemberships$final < 0 )
  neednewid_s <- sort( unique( tempMemberships$final[neednewid_n] ) )
  for ( name in neednewid_s ) {
    masterID <- masterID + 1
    tempMemberships[ tempMemberships$final==name,'final'] <- masterID
  }
  attr( tempMemberships, 'masterID' ) <- masterID
  return( tempMemberships )
}
