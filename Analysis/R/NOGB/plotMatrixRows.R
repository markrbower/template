plotMatrixRows <- function( data, color ) {
  # Help from http://www.howtobuildsoftware.com/index.php/how-do/bhe/r-ggplot2-ggplot-equivalent-for-matplot
  library( ggplot2 )
  library( reshape2 )
  library( plyr )
  
  nrows <- nrow( data )
  ncols <- ncol( data )
  
  dimnames(data) = list(seq(1,nrow(data)), seq(1,ncol(data)))
  plot_data <- melt(data)
  plot_data <- rename( plot_data, c("Var1"="row","Var2"="col") )

  #plot
  a <- max( .01, min( nrows/100, 1 ) )
  p <- ggplot(plot_data, aes(x=col,y=value,group=row)) + geom_line(colour=color,alpha=a)

  return( p )
}
