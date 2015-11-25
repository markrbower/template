uUTC_from_samp <- function( samp, TOC, header) {
  uUTC = zeros(size(samp));
  
  indx = binarySearch(mef_TOC(3,:), samp, -1, -1, []);
  
  for j=1:length(samp)
  if samp(j) > header.number_of_samples
  error(['Sample number ' num2str(samp(j)) ' is beyond the end of the file.']);
  else
    if isempty(indx(j))
  indx(j) = 1;
  end
  uUTC(j) = mef_TOC(1, indx(j)) + 1000000*(samp(j) - mef_TOC(3, indx(j)))/header.sampling_frequency;
  end
  end
  return;
  end  
}