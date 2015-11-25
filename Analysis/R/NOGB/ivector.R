ivector <- function(x, ...) {
   i <- 1
   it <- idiv(length(x), ...)

   nextEl <- function() {
     n <- nextElem(it)
     ix <- seq(i, length=n)
     i <<- i + n
     x[ix]
   }

   obj <- list(nextElem=nextEl)
   class(obj) <- c('ivector', 'abstractiter', 'iter')
   obj
}
