#' Does Trap approimation 
#'
#' 
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#' @param StartToEnd
#'
#' @return object of class trap
#' @author Jack Ploshnick
#' @note Please Work 
#' @examples
#' 

#' @seealso \code{\link{subtractSquares}}
#' @rdname trapfunction
#' @aliases addSquares,ANY-method
#' @export

trap<- function(x,y,StartToEnd){
  a= x[StartToEnd[1]]
  b= x[tail(StartToEnd, n=1)]
  n= as.numeric(length(StartToEnd))
  h=((b-a)/n)
  
  first_value= y[1]
  last_vlaue= tail(y, n=1)
  middle_vlaues= 2* y[2:(length(x)-1)]
  
  answer= (h/2) * sum(first_value, last_vlaue, middle_vlaues)
  
  return(answer)
  
}
