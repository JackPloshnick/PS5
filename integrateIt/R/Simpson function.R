#' Does simpson approximation 
#'
#' 
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#' @param StartToEnd A numeric
#'
#' @return An object of class Trap or Simpson
#' @author Jack Ploshnick
#' @note Please Work 
#' @examples
#' 

#' @seealso \code{\link{subtractSquares}}
#' @rdname simpsonfunction
#' @aliases addSquares,ANY-method
#' @export
simpsons <- function(x,y,StartToEnd){
  a= x[StartToEnd[1]]
  b= x[tail(StartToEnd, n=1)]
  n= as.numeric(length(StartToEnd)-1)
  h=((b-a)/n)
  
  first_value= y[head(StartToEnd, n=1)]
  last_vlaue= y[tail(StartToEnd, n=1)]
  second_to_last= 4* y[(tail(StartToEnd, n=1)-1)]
  middle_vlaues= y[(head(StartToEnd, n=1)+1):(tail(StartToEnd, n=1)-2)]
  
  odd_middles= 4*middle_vlaues[seq(1,length(middle_vlaues),2)]
  
  even_middles= 2*middle_vlaues[seq(0,length(middle_vlaues),2)]
  
  
  answer= (h/3)* (first_value + sum(odd_middles) + sum(even_middles) + second_to_last + last_vlaue)
  
  if(length(x)!= length(y)){
    stop("lengths are not equal")
  }
  
  else{
    return(answer)}
}
