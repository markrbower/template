markAsProcessed <- function( db, filename ) {
  library( RMySQL )
  
  query <- paste0( 'insert ignore into progress (filename) values (\'',filename,'\';' )
    
  print( query )
  
  dbSendQuery( db, query )
    
}
