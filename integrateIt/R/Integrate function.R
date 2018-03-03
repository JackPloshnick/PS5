#' Integrates
#'
#' 
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#' @param start_end_values A numeric
#' @param test a character 
#'
#' @return An object of class Trap or Simpson
#' @author Jack Ploshnick
#' @note Please Work 
#' @examples
#' 

#' @seealso \code{\link{subtractSquares}}
#' @rdname integratefunction
#' @aliases addSquares,ANY-method
#' @export
setGeneric("integrateIt", def = function(x,y,start_end_values,test){
  standardGeneric("integrateIt")
})


setMethod("integrateIt", definition = function(x,y,start_end_values, test ){
  if(test == "Trap"){
    result= new("Trapezoid", x_values= x, y_values= y, estimate=
                  trap(x, y, start_end_values)) 
  }
  
  if(test== "Simpson"){
    result= new("Simpson", x_values= x, y_values= y, estimate=
                  simpsons(x, y, start_end_values)) 
  }
  
  
  return(result)
})





