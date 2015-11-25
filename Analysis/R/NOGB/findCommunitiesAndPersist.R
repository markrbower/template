findCommunitiesAndPersist <- function( db, grph, update, subject, channel, seizureUsed, persist_table, drops, flag ) {
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/findingAllCommunities.R')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/pluralities.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/updateVineNbr.R')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/checkingDatabaseUpdate.R')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/determineClusterType.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/persistLargestGroup.R')
  
  tempdf <- attr( persist_table, 'tempdf' )
  tdfx <- attr( persist_table, 'tdfx' )
  
  # Find communities.
  if ( length(drops)>0 ) {
    # More complicated for group drops:
    # 1. find communities and call them "temps".
    tempMemberships <- findingAllCommunities( grph, "infomap" )
    # 2. find pluralities of existing memberships within temps.
    allMemberships <- pluralities( grph, tempMemberships )
    attr( grph, 'masterID' ) <- attr( allMemberships, 'masterID' )
    # 3. update graph numbering
    grph <- updateAllVineNbrs( grph, allMemberships )
    
    # 4. Store the dropped elements to the temporary dataframe.
    iter_drops <- ihasNext( iter( drops ) )
    masterID <- attr( grph, 'masterID' )
    while ( hasNext( iter_drops ) ) {
      nA <- nextElem( iter_drops )
      tA <- as.numeric( get.vertex.attribute(grph,'name',nA) )
      # Store to temporary dataframe
      clusterid <- get.vertex.attribute(grph,'vine',nA)
      waveform <- get.vertex.attribute(grph,'waveform',nA)
      tempdf[tdfx[1],] <- c(subject,channel,tA,paste0(unlist(waveform),collapse=","),clusterid,seizureUsed)
      tdfx <- tdfx[-1]
      if ( length(tdfx) < 100 ) {
        result <- persistLargestGroup( db, tempdf, tdfx, persist_table, update )
        update <- result$update
        tdfx <- result$tdfx
      }
    }
    # 5.a. Go through the drops, again, find clusters have no remaining members in the graph and can be persisted.
    # Get a list of 'final' vines.
    candidates <- list()
    iter_drops <- ihasNext( iter( drops ) )
    while ( hasNext( iter_drops ) ) {
      nA <- nextElem( iter_drops )
      vA <- as.numeric( get.vertex.attribute(grph,'vine',nA) )
      candidates <- union( candidates, vA )
    }
    grph <- delete.vertices( grph, drops )
    # 5.b For each of these determine whether any others exist in the graph.
    for ( cand in candidates ) {
      if ( !(cand %in% V(grph)$vine) ) { # ... this vine number is done.
        # Persist entries with clusterid==cand in tempdf.
        idx <- which( tempdf[,'clusterid'] == cand )
        
        # Is there enough information to update the noise threshold?
        tempdf <- adjustNoiseThreshold( tempdf )
        noiseThreshold <- attr( tempdf, 'noiseThreshold' )

        type <- determineClusterType( tempdf, idx )
        for ( x in idx ) {
          subject <- tempdf[x,'subject']
          channel <- tempdf[x,'channel']
          waveform <- tempdf[x,'waveform']
          seizureUsed <- tempdf[x,'seizureUsed']
          t <- tempdf[x,'time']
          update <- checkingDatabaseUpdate( db, update, type, subject, channel, t, waveform, seizureUsed, cand, persist_table )
          # Make this entry available again
          tdfx <- union( tdfx, x ) # You don't need to erase old values from tempdf.
        }
      }
    }
    
    if ( flag == 1 ) { # flush the update strings
      if ( (update$signalUpdateCount %% update$limit) != 0 ) {
        updateString <- update$signalUpdateString
        updateString <- paste0( updateString, " ON DUPLICATE KEY UPDATE clusterid=VALUES(clusterid);" );
        #        print( paste0( updateString ) )
        dbSendQuery( db, updateString )
      }
      if ( (update$noiseUpdateCount %% update$limit) != 0 ) {
        updateString <- update$noiseUpdateString
        updateString <- paste0( updateString, " ON DUPLICATE KEY UPDATE clusterid=VALUES(clusterid);" );
        #        print( paste0( updateString ) )
        dbSendQuery( db, updateString )
      }
    }
    
    attr( grph, 'masterID' ) <- masterID
  }
  result <- list( graph=grph, update=update, tdfx=tdfx )
  return( result )
}
