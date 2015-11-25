iterOfIter <- function() {
  library( iterators )
  library( itertools )
  
  L <- list()
  for ( n in seq(1,5) ) {
    L <- append( L, iter(list(1,2,3)) )
  }
  iter(L)
}
