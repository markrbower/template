getNVsubjectFromFilename <- function( filename ) {
  str_extract( case$channel_name, '[0-9A-Za-z]+_[0-9A-Za-z]+_[0-9A-Za-z]+' )
}