computeTimeConstraints <- function( case, info, hourWindow ) {
  # This should be tied to "filterCases" in "MEF_analysisLoop".
  # Perhps I could make a constraint structure of some type that could be passed to both functions?
  # Start with a vector of 'start' and 'stop' times.
  
  result <- vector()
  result['isValid'] <- 0
  usWindow <- 1E6 * 3600 * hourWindow
  
  # If there are no discontinuities in the desired window:
  D <- info$discontinuities
  dt <- abs( D - case$event_start )
  if ( min(dt) > usWindow ) {
    result['start'] <- case$event_start - usWindow
    result['stop'] <- case$event_start + usWindow
    result['usWindow'] <- usWindow
    result['isValid'] <- 1
  }
  return( result )
}
