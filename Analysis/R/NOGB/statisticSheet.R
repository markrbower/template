statisticSheet <- function( subject, channel, seizureTime ) {
#  statisticSheet( 1241, 'NVC1001_23_002_16', 128298045238435 )
  library( ggpubr )
  
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/multiplot.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/selectWaveformsFor.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/statisticSheet.R')
    
  resultset <- selectWaveformsFor( subject, channel, seizureTime )
  signal_time <- resultset$signal_time
  signal_waveform <- resultset$signal_waveform
  signal_w_id <- resultset$signal_id
  signal_seizureUsed <- resultset$signal_seizureUsed
  noise_time <- resultset$noise_time
  noise_waveform <- resultset$noise_waveform
  noise_w_id <- resultset$noise_id
  noise_seizureUsed <- resultset$noise_seizureUsed

  p1 <- waveformPolaritySubsetPlot( signal_waveform, 21, 150, 'green' )
  p2 <- waveformPolaritySubsetPlot( signal_waveform, 18, 150, 'green' )
  p3 <- waveformPolaritySubsetPlot( noise_waveform, 21, 150, 'red' )
  p4 <- waveformPolaritySubsetPlot( noise_waveform, 18, 150, 'red' )
  
  filename <- print( paste0( "~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Presentation/Figures/statisticSheet/infoplot_",subject,'_',channel,'_',seizureTime,'.pdf' ) )
  pdf( filename )
  p <- ggarrange(p1, p2, p3, p4, nrow=2, ncol=2)
  print( p )
  dev.off()
  
}
