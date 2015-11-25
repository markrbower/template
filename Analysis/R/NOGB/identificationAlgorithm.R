identificationAlgorithm <- function( db, filename, password, subject, channel, seizureUsed, persist_table, timeConstraints, info, correlationWindow ) {
  # Computes detections for a window of size 'timeConstraints' around a single seizure
  library( iterators )
  library( itertools )
  library( igraph )
#  library( meftools )
#  library( sqltools )
#  library( igraphtools )
  
  #print( "made it" )
  
  source('~/Dropbox/Documents/Concepts/2018_07_27_meftools/Analysis/R/MEFcont.R')
  Rcpp::sourceCpp('/Users/m063341/Dropbox/Documents/Concepts/2018_07_27_meftools/Analysis/C/decomp_mef.cpp')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/findPeaksAddToGraph.R')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/findDrops.R')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/findCommunitiesAndPersist.R')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/updateAllVineNbrs.R')
#  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/identifySignalsByCounts.R')
  
  # Set up the graph for this window.
  grph <- graph(edges=NULL,n=NULL,directed=FALSE)
  attr( grph, 'masterID' ) <- 0
  tempdf <- data.frame(matrix(nrow = 10000, ncol = 6))
  attr( tempdf, 'noiseThreshold' ) <- 75
  colnames( tempdf ) <- c( 'subject', 'channel', 'time', 'waveform', 'clusterid', 'seizureUsed')
  tdfx <- seq(1,10000)
  attr( persist_table, 'tempdf' ) <- tempdf
  attr( persist_table, 'tdfx' ) <- tdfx
  #print( paste0( "1. ", attr( grph, 'masterID' ) ) )
  
  # Initialize the update string ...
  vineCount <- 1
  signalUpdateCount <- 0
  noiseUpdateCount <- 0
  updateLimit <- 100  # Check each update to make sure you aren't duplicating entries.
  signalUpdateString <- paste0( 'INSERT into P_1hour (subject,channel,time,waveform,clusterid,seizureUsed) VALUES ' )
  noiseUpdateString <- paste0( 'INSERT into N_1hour (subject,channel,time,waveform,clusterid,seizureUsed) VALUES ' )
  update <- list( signalUpdateString=signalUpdateString, signalUpdateCount=signalUpdateCount, noiseUpdateString=noiseUpdateString, noiseUpdateCount=noiseUpdateCount, vineCount=vineCount, limit=updateLimit )
  
  iter_conts <- MEFcont( filename, password, window=timeConstraints, info=info )
  while ( hasNext( iter_conts ) ) { # across contiguous blocks
    iter_data <- nextElem( iter_conts )
    result <- tryCatch( {
      while ( hasNext( iter_data ) ) {
        data <- nextElem( iter_data )
        grph <- findPeaksAddToGraph( grph, data, correlationWindow )
        drops <- findDrops( grph, (10*60*1e6) )
        result <-  findCommunitiesAndPersist( db, grph, update, subject, channel, seizureUsed, persist_table, drops, 0 )
        grph <- result$graph
        update <- result$update
        tdfx <- result$tdfx
      }
    }, error = function(e) {
      line <- paste0( e )
    }
    )
  } # end block
  print( paste0( 'Done with block' ) )

  # Store all
  drops <- V(grph)
  grph <- findCommunitiesAndPersist( db, grph, update, subject, channel, seizureUsed, persist_table, drops, 1 )
  
  rm( data )
  rm( update )
  rm( grph )
  rm( tempdf )
  rm( tdfx )
  gc()
  
  # Identify signals
#  identifySignalsByCounts()
  
}
