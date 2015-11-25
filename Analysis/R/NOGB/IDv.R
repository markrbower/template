IDv <- function( g, name ) {
  library(igraph)
  
  idx <- which(vertex_attr(g,"name")==name)
  if ( length(idx) == 0 ) {
    idx <- 0
  }
  return( idx )
}  