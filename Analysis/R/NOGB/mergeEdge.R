mergeEdge <- function( grph, from, to, W, defaults ) {
  # Based on Neo4j's "merge" command.
  library(igraph)
  
  if ( !exists( "defaults" ) ) {
    defaults = list()
  }
  
  # If the named node currently exists, returns the ID for that node.
  # If not, creates a new node with that name and returns it's ID.
  if ( IDv( grph, from ) == 0 ) {
    grph <- add_vertices( grph, 1, name=as.character(from) )
    cnt <- 0
    while ( cnt < length(defaults) ) {
      grph <- set_vertex_attr(grph, defaults[cnt+1], IDv(grph,from), defaults[cnt+2] )
      cnt <- cnt + 2
    }
  }
  if ( IDv( grph, to ) == 0 ) {
    grph <- add_vertices( grph, 1, name=as.character(to) )
    cnt <- 0
    while ( cnt < length(defaults) ) {
      grph <- set_vertex_attr(grph, defaults[cnt+1], IDv(grph,to), defaults[cnt+2] )
      cnt <- cnt + 2
    }
  }
  from_idx <- IDv( grph, from )
  to_idx <- IDv( grph, to )
  ce <- get.edge.ids( grph, c(from,to) )
  if ( ce == 0 ) {
    grph <- add_edges( grph, c(from_idx,to_idx), weight=W )
  }
  return( grph )
}
