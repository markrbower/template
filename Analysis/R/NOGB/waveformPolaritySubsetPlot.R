waveformPolaritySubsetPlot <- function( waveformMatrix, evalIdx, threshold, color ) {
  # Plot detections based on the polarity and temporal location of the peak.
  library( ggpubr )
  
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/multiplot.R')
  
  if ( threshold > 0 ) {
    idx <- which( waveformMatrix[,evalIdx] > threshold )
  } else {
    idx <- which( waveformMatrix[,evalIdx] < threshold )
  }
  
  # Limit the number of plotted indices. (Otherwise, things hang on my wimpy laptop!)
  if ( length(idx) > 10000 ) {
    idx <- sample( idx, 10000, replace=FALSE )
  }
  
  # Plot waveforms with rate and CC.
  if ( length(idx) > 1 ) {
    p1 <- plotMatrixRows( waveformMatrix[idx,], color )
    cm <- combn( idx, 2 )
    CC <- data.frame( cc=sapply( seq(1,ncol(cm)), function(x) cor( waveformMatrix[cm[1,x],], waveformMatrix[cm[2,x],] ) ) )
    p2 <- ggplot( CC, aes(x=cc)) + geom_histogram(bins=50)
    p <- ggarrange( p1, p2, ncol=2 )
  } else {
    p <- NULL
  }
  
  return( p )
}
