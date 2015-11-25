findPeaksAddToGraph <- function( grph, data, correlationWindow ) {
  library(signal)
  library(igraph)
  library(iterators)
  library(itertools)
  
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/isLocalPeak_IIS.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/uUTC_from_samp.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/IDv.R')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/mergeEdgeToGraph.R')
#  print( paste0( "3.1 ", attr( grph, 'masterID' ) ) )
  masterID <- attr( grph, 'masterID' )
  
  cc_th <- 0.6
  ed_th <- 2.0
  
  skipSize <- 1
  idx <- seq(from=-10,to=10,by=1)
  local_width <- 100
  
  # Timestamp of the first value
  s0 <- attr( data, 's0' )
  t0 <- attr( data, 't0' )
  dt <- attr( data, 'dt' )
  info <- attr( data, 'info' )
  
  # Buffer, filter and downsample
  # - create buffer
  bufferSizePower <- ceiling( log2( length(data) ) )
  bufferSize <- 2^bufferSizePower
  buffer <- vector( mode='double', length=bufferSize )
  # - fill in buffer
  padSize <- round( ( bufferSize - length(data) ) / 2 )
  buffer[1:padSize] <- rev( data[1:padSize] )
  dataIdx <- seq(1,length(data))
  buffer[dataIdx + padSize] <- data
  padIdx <- seq( (padSize + length(dataIdx) + 1),length(buffer) )
  dataIdx <- seq( (length(data)-length(padIdx)+1), length(data) )
  buffer[padIdx] <- rev(data[dataIdx])
  # - filter and unpack
  bfc <- butter( 5, c(.01,.25), type="pass" )
  filt_buffer <- filtfilt( bfc, buffer )
  dataIdx <- seq( from=(padSize+1), to=(padSize + length(data)), by=skipSize )
  filt_data <- filt_buffer[dataIdx]
  
  # Find candidate peaks
  dw = diff(filt_data);
  sdw = sign(dw);
  dsdw = diff(sdw);
  C = which( dsdw>0 | dsdw<0 ) + 1;
  N = length(C);
  
  bad = which( C < (abs(min(idx))+1) | C > ((length(filt_data)-max(idx))-1) );
  if ( length(bad) > 0 ) {
    C <- C[-bad];
  }
  
  # Find peaks
#  print( paste0( "3.2 ", attr( grph, 'masterID' ) ) )
  peakIdxCnt <- 0
  iterC <- ihasNext( iter( C ) )
  while( hasNext( iterC ) ) {
    c_ <- nextElem(iterC)
    if ( isLocalPeak_IIS( filt_data, c_, local_width ) ) { # IIS
      peakIdxCnt = peakIdxCnt + 1;
      wvfrm = filt_data[c_+idx];
      # Add to graph
#      str = paste0( wvfrm, collapse=',' );
      utc = uUTC_from_samp( (c_*skipSize)+s0, info$ToC, info$header); # re-instate downsampling factor
      utc = round( utc );
      # Store this node
#      print( paste0( "3.3 ", attr( grph, 'masterID' ) ) )
      if ( IDv( grph, utc ) == 0 ) {
        grph <- add_vertices( grph, 1, name=as.character(utc) )
#        print( paste0( "3.4 ", attr( grph, 'masterID' ) ) )
        cnt <- 0
        defaults <- list("vine",-1, "waveform", wvfrm )
        while ( cnt < length(defaults) ) {
          grph <- set_vertex_attr(grph, defaults[cnt+1], IDv(grph,utc), defaults[cnt+2] )
#          print( paste0( "3.45 ", attr( grph, 'masterID' ) ) )
          cnt <- cnt + 2
        }
        # Compute CC for nodes within the 'correlation window'.
        times <- sort( as.numeric( names( V(grph) ) ) )
        ccTimes <- ihasNext( iter( times[which((utc-times)<correlationWindow)] ) )
        while ( hasNext(ccTimes) ) {
          t <- nextElem( ccTimes )
          if ( utc != t ) {
            priorWvfrm <- unlist( get.vertex.attribute( grph, 'waveform', index=IDv(grph,t) ) )
            cc <- cor(wvfrm,priorWvfrm)
            ed <- sqrt( sum((wvfrm-priorWvfrm)*(wvfrm-priorWvfrm)) ) / (sqrt( sum((priorWvfrm)*(priorWvfrm)) ) )
            if ( cc > cc_th & ed < ed_th ) {
  #            print( paste0( "3.5 ", attr( grph, 'masterID' ) ) )
              grph <- mergeEdgeToGraph( grph, utc, t, (cc+1)/2, ed )
  #            print( paste0( "3.6 ", attr( grph, 'masterID' ) ) )
            }
          }  
        }
      }
    }
  }
  attr(grph, 'masterID') <- masterID
  return( grph )
}
