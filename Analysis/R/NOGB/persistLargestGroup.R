persistLargestGroup <- function( db, grph, tempdf, tdfx, persist_table, update ) {
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/checkingDatabaseUpdate.R')
  
  counts <- table( tempdf[,'clusterid'] )
  idx <- which( counts==max(counts) )
  target <- names( counts[idx[1]] )
  idx <- which( tempdf[,'clusterid'] == target )
  for ( x in idx ) {
    subject <- tempdf[x,'subject']
    channel <- tempdf[x,'channel']
    waveform <- tempdf[x,'waveform']
    seizureUsed <- tempdf[x,'seizureUsed']
    t <- tempdf[x,'time']
    update <- checkingDatabaseUpdate( db, update, subject, channel, t, waveform, seizureUsed, persist_table )
  }
  
  return_df <- data.frame( update=update, tdfx=tdfx )

  return( return_df )
}
