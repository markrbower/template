MEF_analysisLoop <- function( password ) {
  library( RMySQL )
  library( doParallel )
  library( foreach )

  source('~/Dropbox/Documents/Concepts/2018_07_27_meftools/Analysis/R/SQLiter.R')

  hourWindow <- 1
  persist_table <- "P_1hour"
  correlationWindow <- 60*1E6

#  cl<-makeCluster(2,outfile="")
#  registerDoParallel(cl)
  
  db <- dbConnect( MySQL(), user='root', password='', dbname='markdb_projectKoala', host='localhost' );
  query <- 'select distinct msel,channel_name,event_start from seizures where msel=1241 and channel_name=\'NVC1001_23_002_01\' and event_start=128298045238435 order by event_start,channel_name,msel;'
#  query <- 'select distinct msel,channel_name,event_start from seizures order by event_start,channel_name,msel;'
  cases <- SQLiter( db, query )
  dbDisconnect( db )
  
  basedir <- '/Volumes/Oyen_1TB/RawData/NV_Human/'
  
#  info <- list( filename='0_0_0') # prime this variable
  
#  foreach (case=cases ) %dopar% { # a case is a named list: subject, channel and event_start
  while ( hasNext(cases) ) { # a case is a named list: subject, channel and event_start
    case <- nextElem( cases )
    library( RMySQL )
    
    source('~/Dropbox/Documents/Concepts/2018_07_27_meftools/Analysis/R/mef_info.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/filenameFromCase.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/computeTimeConstraints.R')
    source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/getNVsubjectFromFilename.R')

    filename <- filenameFromCase( basedir, case )
    info <- mef_info( c(filename,password) )

    timeConstraints <- computeTimeConstraints( case, info, hourWindow )
    
    if ( timeConstraints['isValid'] ) {
  
      source('~/Dropbox/Documents/Concepts/2018_06_08_LearningRcpp/Analysis/R/LearningRcpp/notProcessed.R')
      debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/identificationAlgorithm.R')
      debugSource('~/Dropbox/Documents/Concepts/2018_06_08_LearningRcpp/Analysis/R/LearningRcpp/markAsProcessed.R')
      
      db <- dbConnect( MySQL(), user='root', password='', dbname='markdb_projectKoala', host='localhost' );
      if ( notProcessed( db, filename ) ) {
        identificationAlgorithm( db, filename, password, case$msel, case$channel, case$event_start, persist_table, timeConstraints, info, correlationWindow )
        count_energy <- visualizeChannelStatistics( case$msel, case$channel, case$event_start )

        bootstrapNoiseEstimator( db, subject, channel, seizureUsed, persist_table, count_energy )
        
        markAsProcessed( db, case )
      }
      dbDisconnect( db )
    }
    rm( info )
    gc()
  }

}
