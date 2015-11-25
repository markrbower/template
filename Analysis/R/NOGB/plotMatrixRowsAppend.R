plotMatrixRowsAppend <- function( p, data, color ) {
  # Help from http://www.howtobuildsoftware.com/index.php/how-do/bhe/r-ggplot2-ggplot-equivalent-for-matplot
  library( ggplot2 )
  library( reshape2 )
  library( plyr )
  
  dimnames(data) = list(seq(1,nrow(data)), seq(1:ncol(data)))
  plot_data <- melt(data)
  plot_data <- rename( plot_data, c("Var1"="row","Var2"="col") )
  
  #plot
  p <- p + geom_line( data=plot_data, aes(x=col,y=value,group=row), colour=color, alpha=.2 )
  
  return( p )
}
