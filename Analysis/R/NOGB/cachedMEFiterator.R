cachedMEFiterator <- function( filename, password, ... ) {
  args <- list(...)
  
  info <- mef_info( c(filename,password) )

  if (is.null(args[['size']]) ) {
    # If size not given, set to roughy 100k samples.
    nBlocks <- ceiling( 100E3/info$header$maximum_block_length )
    size <- info$ToC[3,(nBlocks+1)]
  }
  
  # Check that you won't read over any discontinuities.
  cache$filename <- filename
  cache$password <- password
  cache$info <- info
  cache$samplePtr <- 0
  cache$sampleLimit <- size
#  data <- decomp_mef( c(filename,0,64000,password) )
  return( cache )  
}
