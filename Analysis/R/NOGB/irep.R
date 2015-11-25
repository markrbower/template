irep <- function(x, times) {
   nextEl <- function() {
     if (times > 0)
       times <<- times - 1
     else
       stop('StopIteration')
    
      x
    }
  
    obj <- list(nextElem=nextEl)
    class(obj) <- c('irep', 'abstractiter', 'iter')
    obj
}