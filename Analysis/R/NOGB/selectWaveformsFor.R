selectWaveformsFor <- function( subject, channel, seizureTime ) {
  # Generates plots of all 'signal' and 'noise' waveforms along with count vs. energy plots
  # for a single subject, channel and seizure time.
  # visualizeIdentificationsFor( 1241, 'NVC1001_23_002_06', 128298045238435 )
  library( RMySQL )
  library( ggplot2 )
  
  # Connect to the database
  db <- dbConnect( MySQL(), user='root', password='', dbname='markdb_projectKoala', host='localhost' )
  
  # Retrieve all 'signal' waveforms for a given subject, channel and seizureTime.
  query <- paste0( 'select time,waveform,clusterid,seizureUsed from P_1hour where subject=',subject,' and channel=\'',channel,'\' and seizureUsed=',seizureTime, ';' );
  signal_resultset <- dbGetQuery( db, query )
  signal_time <- signal_resultset$time
  tmp <- signal_resultset$waveform
  signal_clusterid <- signal_resultset$clusterid
  signal_seizureUsed <- signal_resultset$seizureUsed
  M <- length(tmp)
  N <- length( as.numeric( unlist( strsplit( gsub("[^-0-9,\\.]","",tmp[[1]]), ',' ) ) ) )
  signal_waveform <- matrix( nrow=M, ncol=N )
  for ( j in seq(1,length(tmp)) ) {
    signal_waveform[j,] <- as.numeric( unlist( strsplit( gsub("[^-0-9,\\.]","",tmp[[j]]), ',' ) ) )    
  }
  
  # Retrieve all 'noise' waveforms for a given subject, channel and seizureTime.
  query <- paste0( 'select time,waveform,clusterid,seizureUsed from N_1hour where subject=',subject,' and channel=\'',channel,'\' and seizureUsed=', seizureTime, ' and clusterid>0;' );
  noise_resultset <- dbGetQuery( db, query )
  noise_time <- noise_resultset$time
  tmp <- noise_resultset$waveform
  noise_clusterid <- noise_resultset$clusterid
  noise_seizureUsed <- noise_resultset$seizureUsed
  M <- length(tmp)
  N <- length( as.numeric( unlist( strsplit( gsub("[^-0-9,\\.]","",tmp[[1]]), ',' ) ) ) )
  noise_waveform <- matrix( nrow=M, ncol=N )
  for ( j in seq(1,length(tmp)) ) {
    noise_waveform[j,] <- as.numeric( unlist( strsplit( gsub("[^-0-9,\\.]","",tmp[[j]]), ',' ) ) )    
  }
  
  # Disconnect from the database.
  dbDisconnect( db )
  
  result <- list( signal_time=signal_time, signal_waveform=signal_waveform, signal_id=signal_clusterid, signal_seizureUsed=signal_seizureUsed, noise_time=noise_time, noise_waveform=noise_waveform, noise_id=noise_clusterid, noise_seizureUsed=noise_seizureUsed )
  return( result )
}
