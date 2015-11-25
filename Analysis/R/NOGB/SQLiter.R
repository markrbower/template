SQLiter <- function( db, query ) {
  # Arguments:
  # db    : MySQL database connection
  # query : "select ..." - it is assumed that this will always be a "select" query.
  #
  library( RMySQL )
  library( iterators )
  library( itertools )
  i <- 1
    
  resultset <- dbGetQuery( db, query )

  it <- iter( resultset, by="row" )

  nextEl <- function() {
    n <- nextElem(it)
  }
  
  obj <- list(nextElem=nextEl)
  class(obj) <- c('SQLiter', 'abstractiter', 'iter')
  returnable <- ihasNext(obj)
  attr( returnable, "size" ) <- nrow( resultset )
  return( returnable )
}
