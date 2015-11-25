determineClusterType <- function( tempdf, idx ) {
  # Does this qualify as a noise event? The Iron Law of Noise.
  # Peak > 75 µV
  # Rate > 1/300 Hz (at least one event every 5 min)
  # Rate < 5 Hz
  # Count > 2
  type <- 'noise'
  t <- sort( tempdf[idx,'time'] )
  N <- length(t)
  if ( N > 2 ) {
    tmp <- tempdf[idx,'waveform']
    M <- length(tmp)
    N <- length( as.numeric( unlist( strsplit( gsub("[^-0-9,\\.]","",tmp[[1]]), ',' ) ) ) )
    signal_waveform <- matrix( nrow=M, ncol=N )
    for ( j in seq(1,length(tmp)) ) {
      signal_waveform[j,] <- as.numeric( unlist( strsplit( gsub("[^-0-9,\\.]","",tmp[[j]]), ',' ) ) )    
    }
    peak <- mean( abs( signal_waveform[,11] ) )
    if ( peak > 50 ) {
      rate <- 1E6 * (M-1) / (t[M] - t[1])
      if ( rate > (1/300) & rate < 5 ) {
        type <- 'signal'
      }
    }
  }
  return(type)
}
