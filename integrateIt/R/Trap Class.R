#' Creates Trap class
#'
#' 
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#' @param estimate a numeric
#' 
#' @return makes Trap class
#' @author Jack Ploshnick
#' @note Please Work 
#' @examples
#' 

#' @seealso \code{\link{subtractSquares}}
#' @rdname trapclass
#' @aliases addSquares,ANY-method
#' @export



setClass(Class="Trapezoid",  #Sets S4 class of door
         representation = representation(
           x_values = "numeric", # three slots as specified in problem set
           y_values = "numeric", 
           estimate = "numeric"
         ),
         prototype = prototype(
           x_values = c(), #default values are empty 
           y_values = c(),
           estimate = c()
         )
)

setValidity("Trapezoid", function(object){ 
  estimateLength = (length(object@estimate == 1))
  
  valuesLength= (length(object@x_values)== length(object@y_values))
  
  if(!estimateLength | !valuesLength){
    return("Trapezoid not valid")
  }
  
})


setMethod("initialize", "Trapezoid", function(.Object, ...) { #initilize method 
  value = callNextMethod()
  validObject(value)
  return(value)
})