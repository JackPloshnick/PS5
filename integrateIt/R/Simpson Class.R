#' Class of Simpson
#'
#' 
#'
#' @param x A numeric object
#' @param y A numeric object with the same dimensionality as \code{x}.
#' @param estimate A numeric
#'
#' @return creates Simpson Class
#' @author Jack Ploshnick
#' @note Please Work 
#' @examples
#' 

#' @seealso \code{\link{subtractSquares}}
#' @rdname simpsonclass
#' @aliases addSquares,ANY-method
#' @export

setClass(Class="Simpson",  #Sets S4 class of Simpson
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

setValidity("Simpson", function(object){ 
  estimateLength = (length(object@estimate == 1)) #ensures only one value in estimate 
  
  valuesLength= (length(object@x_values)== length(object@y_values)) #ensures length of x and y are the same 
  
  if(!estimateLength | !valuesLength){ #if either is not true, return error 
    return("Simpson not valid")
  }
  
})



setMethod("initialize", "Simpson", function(.Object, ...) { #initilize method 
  value = callNextMethod()
  validObject(value)
  return(value)
})

