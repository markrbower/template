checkingDatabaseUpdate <- function( db, update, type, subject, channel, tA, waveform, seizureUsed, vineNbr, P_table ) {
  library( RMySQL )
  library( igraph )

  updateLimit <- update[["limit"]]

# masterID should take care of this.
#  # Check whether update$vineCount needs to be incremented
#  if ( vineNbr == update$vineCount ) {
#    update$vineCount <- update$vineCount + 1
#  }

  if ( type == 'signal' ) {
    signalUpdateString <- update$signalUpdateString
    signalUpdateCount <- update$signalUpdateCount
    if ( ( signalUpdateCount %% updateLimit )>0 ) {
      signalUpdateString <- paste0( signalUpdateString, "," )
    }
    signalUpdateString <- paste0( signalUpdateString, " ( ", subject, ", '", channel, "', ",tA, ", '", waveform, "', ", vineNbr, ", ", seizureUsed, " )" )
    signalUpdateCount <- signalUpdateCount + 1
    #print( updateCount )
    if ( ( signalUpdateCount %% updateLimit ) == 0 ) {
      print( signalUpdateCount )
      signalUpdateString <- paste0( signalUpdateString, " ON DUPLICATE KEY UPDATE clusterid=VALUES(clusterid);" );
      #             print( updateString )
      #            Sys.sleep(1)
      #print(updateString)
      dbSendQuery( db, signalUpdateString )
      # Re-initialize
      signalUpdateString <- paste0( 'INSERT into ', P_table, ' (subject,channel,time,waveform,clusterid,seizureUsed) VALUES ' )
    }
    update$signalUpdateCount <- signalUpdateCount
    update$signalUpdateString <- signalUpdateString
  } else if ( type == 'noise' ) {
    noiseUpdateString <- update$noiseUpdateString
    noiseUpdateCount <- update$noiseUpdateCount
    if ( ( noiseUpdateCount %% updateLimit )>0 ) {
      noiseUpdateString <- paste0( noiseUpdateString, "," )
    }
    noiseUpdateString <- paste0( noiseUpdateString, " ( ", subject, ", '", channel, "', ",tA, ", '", waveform, "', ", vineNbr, ", ", seizureUsed, " )" )
    noiseUpdateCount <- noiseUpdateCount + 1
    #print( updateCount )
    if ( ( noiseUpdateCount %% updateLimit ) == 0 ) {
      print( noiseUpdateCount )
      noiseUpdateString <- paste0( noiseUpdateString, " ON DUPLICATE KEY UPDATE clusterid=VALUES(clusterid);" );
      #             print( updateString )
      #            Sys.sleep(1)
      #print(updateString)
      dbSendQuery( db, noiseUpdateString )
      # Re-initialize
      noiseUpdateString <- paste0( 'INSERT into N_1hour (subject,channel,time,waveform,clusterid,seizureUsed) VALUES ' )
    }
    update$noiseUpdateCount <- noiseUpdateCount
    update$noiseUpdateString <- noiseUpdateString
  }  
  return( update )  
}
