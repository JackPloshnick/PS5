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

trap<- function(x,y,StartToEnd){ #runs trapezoid approximation 
  a= x[StartToEnd[1]] #a is first value in range given
  b= x[tail(StartToEnd, n=1)] #b is last value in range given 
  n= as.numeric(length(StartToEnd)-1) #n is total range -1
  h=((b-a)/n)
  
  first_value= y[head(StartToEnd, n=1)] #first value of y. not multiplied by anyhing 
  last_vlaue= y[tail(StartToEnd, n=1)]# last value of y. not multiplied by anything 
  middle_vlaues= 2* y[ (head(StartToEnd, n=1)+1):(tail(StartToEnd, n=1)-1)] #all other values. multiplied by 2
  
  answer= (h/2) * (first_value + last_vlaue + sum(middle_vlaues)) #sum as specidied in formula 
  
  if(length(x)!= length(y)){ #if unequal lengths, stop 
    stop("lengths are not equal")
  }
  
  else{
    return(answer)}
  
}


