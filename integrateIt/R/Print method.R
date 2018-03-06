#' Prints
#'
#' 
#'
#' @param x A trap or simpson
#'
#' @return A numeric
#' @author Jack Ploshnick
#' @note Please Work 
#' @examples
#' 

#' @seealso \code{\link{subtractSquares}}
#' @rdname printfunction
#' @aliases addSquares,ANY-method
#' @export

setMethod("print", "Trapezoid", #very simple print function. uses x as input value as specidied in help file 
          function(x){
            print(x@estimate)})

setMethod("print", "Simpson",
          function(x){
            print(x@estimate)})

