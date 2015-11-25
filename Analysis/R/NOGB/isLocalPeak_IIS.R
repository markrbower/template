isLocalPeak_IIS <- function( filt_data, c_, width ) {
  width = 100;
  idx = (c_-width):(c_+width);
  bad <- which( idx<1 | idx>length(filt_data) )
  if ( length(bad) > 0 ) {
    idx <- idx[-bad]
  }
  flag = (filt_data[c_] == min(filt_data[idx])) | (filt_data[c_] == max(filt_data[idx]));  
}
