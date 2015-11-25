CMI_hasNext <- function( cache ) {
  value <- cache$samplePtr < cache$info$header$number_of_samples
  if (value) {
    # Load up what will be returned by "next"
  }  
  return( value );  
}
