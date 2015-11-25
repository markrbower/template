visualizeChannelStatistics <- function( subject, channel, seizureTime ) {
  # Generates a PDF showing relevant signal-noise information.
  # Based on 'visualizeIdentificationsFor.R'.
  # visualizeChannelStatistics( 1241, 'NVC1001_23_002_01', 128298045238435 )
  library(ggplot2)
  
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/multiplot.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/selectWaveformsFor.R')
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/selectCountByClusteridFor.R')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/plotMatrixRows.R')
  debugSource('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/plotMatrixRowsAppend.R')
  
  centerIdx <- 11
  
  # Load data from database.
  resultset <- selectWaveformsFor( subject, channel, seizureTime )
  signal_time <- resultset$signal_time
  signal_waveform <- resultset$signal_waveform
  signal_w_id <- resultset$signal_id
  signal_seizureUsed <- resultset$signal_seizureUsed
  noise_time <- resultset$noise_time
  noise_waveform <- resultset$noise_waveform
  noise_w_id <- resultset$noise_id
  noise_seizureUsed <- resultset$noise_seizureUsed
  #load(file='workOnGraphics.RData')
  
  
#  # Switch low-peak 'signal' to noise and high-peak 'noise' to signal.
#  bad <- which( abs(signal_waveform[,centerIdx]) < 50 )
#  good <- which( abs(noise_waveform[,centerIdx]) > 100 )
#  if ( length(bad) >0 ) {
#    signal_waveform_tmp <- signal_waveform[bad,]
#    signal_waveform <- signal_waveform[-bad,]
#    signal_time_tmp <- signal_time[bad]
#    signal_time <- signal_time[-bad]
#    signal_w_id_tmp <- signal_w_id[bad]
#    signal_w_id <- signal_w_id[-bad]
#    signal_seizureUsed_tmp <- signal_seizureUsed[bad]
#    signal_seizureUsed <- signal_seizureUsed[-bad]
#  }  
#  if ( length(good) > 0 ) {
#    noise_waveform_tmp <- noise_waveform[good,]
#    noise_waveform <- noise_waveform[-good,]
#    noise_time_tmp <- noise_time[good]
#    noise_time <- noise_time[-good]
#    noise_w_id_tmp <- noise_w_id[good]
#    noise_w_id <- noise_w_id[-good]
#    noise_seizureUsed_tmp <- noise_seizureUsed[good]
#    noise_seizureUsed <- noise_seizureUsed[-good]
#  }
#  if ( length(good) > 0 ) {
#    signal_waveform <- rbind( signal_waveform, noise_waveform_tmp )
#    signal_time <- rbind( signal_time, noise_time_tmp )
#    signal_w_id <- rbind( signal_w_id, noise_w_id_tmp )
#    signal_seizureUsed <- rbind( signal_seizureUsed, noise_seizureUsed_tmp )
#  }
#  if ( length(bad) > 0 ) {
#    noise_waveform <- rbind( noise_waveform, signal_waveform_tmp )
#    noise_time <- rbind( noise_time, signal_time_tmp )
#    noise_w_id <- rbind( noise_w_id, signal_w_id_tmp )
#    noise_seizureUsed <- rbind( noise_seizureUsed, signal_seizureUsed_tmp )
#  }
  
  # Plot positive-peak signal and noise
  posidx <- which( noise_waveform[,centerIdx] > 0 )
  p1 <- plotMatrixRows( noise_waveform[posidx,], color='magenta' )
  posidx <- which( signal_waveform[,centerIdx] > 0 )
  p1 <- plotMatrixRowsAppend( p1, signal_waveform[posidx,], color='cyan' )
#  print(p1)
  
  # Plot negative-peak signal and noise
  negidx <- which( noise_waveform[,centerIdx] < 0 )
  p2 <- plotMatrixRows( noise_waveform[negidx,], color='magenta' )
  negidx <- which( signal_waveform[,centerIdx] < 0 )
  p2 <- plotMatrixRowsAppend( p2, signal_waveform[negidx,], color='cyan' )
#  print(p2)
  
  # Plot count-vs-energy
  resultset <- selectCountByClusteridFor( subject, channel, seizureTime )
  signal_count <- resultset$signal_count
  signal_id <- resultset$signal_id
  noise_count <- resultset$noise_count
  noise_id <- resultset$noise_id

  signal_id_list <- unique( signal_id )
  signal_energy <- vector( mode = "double", length = length(signal_id_list) )
  signal_count <- vector( mode="integer", length = length(signal_id_list) )
  cnt <- 0
  for ( id in signal_id_list ) {
    cnt <- cnt + 1
    signal_idx <- which( signal_w_id == id )
    if ( length(signal_idx)==1 ) {
      signal_energy[cnt] <- sqrt( sum(signal_waveform[signal_idx,] * signal_waveform[signal_idx,]) )
      signal_count[cnt] <- 1
    } else {
      signal_energy[cnt] <- mean( apply( signal_waveform[signal_idx,], 1, function(x) sqrt( sum( x*x ) ) ) )
      signal_count[cnt] <- length(signal_idx)
    }
  }
  noise_id_list <- unique( noise_id )
  noise_energy <- vector( mode = "double", length = length(noise_id_list) )
  noise_count <- vector( mode="integer", length = length(noise_id_list) )
  cnt <- 0
  for ( id in noise_id_list ) {
    cnt <- cnt + 1
    noise_idx <- which( noise_w_id == id )
    if ( length(noise_idx)==1 ) {
      noise_energy[cnt] <- sqrt( sum(noise_waveform[noise_idx,] * noise_waveform[noise_idx,]) )
      noise_count[cnt] <- 1
    } else {
      noise_energy[cnt] <- mean( apply( noise_waveform[noise_idx,], 1, function(x) sqrt( sum( x*x ) ) ) )
      noise_count[cnt] <- length(noise_idx)
    }
  }
#  bad <- which( signal_count == 1 )
#  if ( length(bad) > 0 ) {
#    signal_count <- signal_count[-bad]  
#    signal_energy <- signal_energy[-bad]  
#  }  
#  bad <- which( noise_count == 1 )
#  if ( length(bad) > 0 ) {
#    noise_count <- noise_count[-bad]  
#    noise_energy <- noise_energy[-bad]  
#  }  
  signals <- data.frame( group='signal', count=log10(signal_count), energy=log10(signal_energy) )
  noises  <- data.frame( group='noise', count=log10(noise_count), energy=log10(noise_energy) )
  count_energy <- rbind( signals, noises )
  
  data <- rbind( noises, signals )
  p3 <- ggplot( data=data, aes( x=count, y=energy, color=group ) ) + 
          geom_point() + 
          scale_colour_manual(values = c("magenta", "cyan")) +
          scale_y_log10() + scale_x_log10()
  
  # Inter-spike intervals
  allSignal_time <- sort( signal_time )
#  hist( log10( diff(allSignals_time) ) )
  allNoise_time <- sort( noise_time )
#  hist( log10( diff(allNoise_time) ) )
  s_isi <- data.frame( group='signal', isi=log10(diff(allSignal_time)) )
  n_isi <- data.frame( group='noise', isi=log10(diff(allNoise_time)) )
  isi <- rbind( s_isi, n_isi )
  p4 <- ggplot( data=isi, aes(x=isi,y=..density..,group=group,colour=group,fill=group ) ) +
        geom_histogram() +
        geom_line(stat='density') +
        facet_grid(group ~ .) +
        scale_fill_manual( values=c("cyan","magenta") ) +
        scale_colour_manual( values=c("black","black") )
  
  dsignal <- signal_time - signal_seizureUsed
  signalTimes <- data.frame( dt=dsignal )
  breaks <- seq( from=-3600*1e6, to=3600*1e6, by=60*1e6 )
  p5 <- ggplot( signalTimes, aes(x=dt) ) + geom_histogram( breaks=breaks )

  # Produce a multiplot and make a PDF
#  filename <- print( paste0( "~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Presentation/Figures/infoplot_",subject,'_',channel,'_',seizureTime,'.pdf' ) )
#  pdf( filename )
#  multiplot(p1, p2, p3, p4, p5, cols=2)
#  dev.off()
  print( multiplot(p1, p2, p3, p4, p5, cols=2) )
  
  return( count_energy )
}
