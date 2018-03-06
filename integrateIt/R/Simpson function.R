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
simpsons <- function(x,y,StartToEnd){#Does simpson approximation  
  a= x[StartToEnd[1]]# first value is first value in range given
  b= x[tail(StartToEnd, n=1)] #last value is last value in range given
  n= as.numeric(length(StartToEnd)-1) #n is total range -1
  h=((b-a)/n)
  
  first_value= y[head(StartToEnd, n=1)] #gives first value of y, not multiplied by anything 
  last_vlaue= y[tail(StartToEnd, n=1)]# gives last value of y, not multiplied by anything 
  second_to_last= 4* y[(tail(StartToEnd, n=1)-1)] #second to last value of y, multiplied by 4
  middle_vlaues= y[(head(StartToEnd, n=1)+1):(tail(StartToEnd, n=1)-2)] #all values that are not the first, second to last or last values
  
  odd_middles= 4*middle_vlaues[seq(1,length(middle_vlaues),2)]#values to be multiplied by 4. The 2,4,6 etc values overall. In orther words, the 1,3,5 etc values of the middle vlaues 
  
  even_middles= 2*middle_vlaues[seq(0,length(middle_vlaues),2)]#values to be multiplied by 2. The 3,5,7 etc values overall. or the 2,4,6 etc values of the middle values 
  
  
  answer= (h/3)* (first_value + sum(odd_middles) + sum(even_middles) + second_to_last + last_vlaue) #sum them all as specified in formula 
  
  if(length(x)!= length(y)){
    stop("lengths are not equal")
  }
  
  else{
    return(answer)}
}
