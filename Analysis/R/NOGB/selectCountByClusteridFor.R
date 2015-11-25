selectCountByClusteridFor <- function( subject, channel, seizureTime ) {
  # Generates plots of all 'signal' and 'noise' waveforms along with count vs. energy plots
  # for a single subject, channel and seizure time.
  # visualizeIdentificationsFor( 1241, 'NVC1001_23_002_06', 128298045238435 )
  library( RMySQL )
  library( ggplot2 )
  
  # Connect to the database
  db <- dbConnect( MySQL(), user='root', password='', dbname='markdb_projectKoala', host='localhost' )
  
  # Retrieve all 'signal' waveforms for a given subject, channel and seizureTime.
  query <- paste0( 'select clusterid,count(*) as count from P_1hour where subject=',subject,' and channel=\'',channel,'\' and seizureUsed=',seizureTime, ' group by clusterid;' );
  signal_resultset <- dbGetQuery( db, query )
  signal_count <- signal_resultset$count
  signal_clusterid <- signal_resultset$clusterid

  # Retrieve all 'noise' waveforms for a given subject, channel and seizureTime.
  query <- paste0( 'select clusterid,count(*) as count from N_1hour where subject=',subject,' and channel=\'',channel,'\' and seizureUsed=', seizureTime, ' and clusterid>0 group by clusterid;' );
  noise_resultset <- dbGetQuery( db, query )
  noise_count <- noise_resultset$count
  noise_clusterid <- noise_resultset$clusterid

  # Disconnect from the database.
  dbDisconnect( db )
  
  result <- list( signal_count=signal_count, signal_id=signal_clusterid, noise_count=noise_count, noise_id=noise_clusterid )
  return( result )
}
