filenameFromCase <- function( basedir, case ) {
  library( stringr )
  
  source('~/Dropbox/Documents/Concepts/2018_07_26_NoiseOutlierGraphBased/Analysis/R/NOGB/getNVsubjectFromFilename.R')
  
  patientDir <- getNVsubjectFromFilename( case$channel_name )
  localname <- paste0( case$channel_name, '.mef' )

  result <- paste0( basedir, file.path( patientDir, localname, fsep = .Platform$file.sep) )
  
  return( result )
}
