notProcessed <- function( db, case ) {
  library( RMySQL )
  
  query <- paste0( 'select done from progress where subject=\'',case$msel,'\' and channel_name=\'', case$channel_name, '\' and timestamp=', case$event_start,';' )
  
  rs <- dbGetQuery( db, query )

  done <- 0
  if ( length(rs$done) > 0 ) {  
    done <- 1
  }
  return( done )
}
