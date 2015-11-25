bootstrapNoiseEstimator <- function( db, subject, channel, seizureUsed, persist_table ) {
  ce <- visualizeChannelStatistics( subject, channel, seizureUsed )
  noise_idx <- which( ce$group=='noise')
  noises <- ce[,noise_idx]
  signal_idx <- which( ce$group=='signal')
  signals <- ce[,signal_idx]
  
  val <- sort( unique( noises$count ) )
  upr <- sapply( val, function(x) {idx <- which(noises$count==x); mean(noises$energy[idx]) + 2*sd(noises$energy[idx])} )


}  
