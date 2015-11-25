generateChannelStatistics <- function() {
  library( RMySQL )
  
  source('~/Dropbox/Documents/Concepts/2018_07_27_meftools/Analysis/R/SQLiter.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/statisticSheet.R')

  conn <- dbConnect( MySQL(), user='root', password='', dbname='markdb_projectKoala', host='localhost' )
  query <- paste0( 'select subject,filename,timestamp from progress;' )
  Qiter <- SQLiter( conn, query )
  
  while ( hasNext(Qiter) ) {
    case <- nextElem( Qiter )
    statisticSheet( case$subject, case$filename, case$timestamp )
  }

  dbDisconnect( conn )
}
