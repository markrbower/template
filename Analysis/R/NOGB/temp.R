temp <- function() {
  library(RMySQL)
  source('~/Dropbox/Documents/Concepts/2017_04_16_CURE_TakingFlight/Analysis/R/TakingFlight/clearAllDBcons.R')
  clearAllDBcons()

  # Open the database.
  db <- dbConnect(MySQL(), user='root', password='', dbname='reviewed_events', host='localhost')
  
  # QUERY:
  query <- sprintf("select distinct msel from iis;” )
  rs <- dbSendQuery( db, query )
  dbresult <- fetch( rs, -1 )
  
  # Package the events to be returned.
  result <- data.frame( msel=integer())
  Nlines <- 

  # Close the database.
  dbDisconnect(db)

  return(result)
}