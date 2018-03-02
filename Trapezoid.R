###figuring out trapezoid

#equations is
#  y= x^2

x<- c(1,2,3,4,5)
y<- c(1,4,9,16,25)

StartToEnd<- c(1:5)

trap<- function(x,y,StartToEnd){
  a= x[StartToEnd[1]]
  b= x[tail(StartToEnd, n=1)]
  n= as.numeric(length(StartToEnd))
  h=((b-a)/n)
  
  first_value= y[1]
  last_vlaue= tail(y, n=1)
  middle_vlaues= 2* y[2:(length(x)-1)]
  
  answer= (h/2) * sum(first_value, last_vlaue, middle_vlaues)
  
  print(answer)
  
}

trap(x,y,StartToEnd)

#################

simpsons <- function(x,y,StartToEnd){
  a= x[StartToEnd[1]]
  b= x[tail(StartToEnd, n=1)]
  n= as.numeric(length(StartToEnd))
  h=((b-a)/n)
  
  first_value= y[1]
  last_vlaue= tail(y, n=1)
  second_to_last= 4* head(tail(y, n=2), n=1)
  middle_vlaues= y[2:(length(x)-2)]
  
  odd_middles= 4*middle_vlaues[seq(1,length(middle_vlaues),2)]
  
  even_middles= 2*middle_vlaues[seq(0,length(middle_vlaues),2)]

  
  answer= (h/3)* sum(first_value, odd_middles, even_middles,second_to_last, last_vlaue)
  
  print(answer)
}

simpsons(x,y,StartToEnd)

############# Making Classes

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

new("Trapezoid", x_values= x, y_values= y, estimate= 33.6)

setMethod("initialize", "Trapezoid", function(.Object, ...) { #initilize method 
  value = callNextMethod()
  validObject(value)
  return(value)
})

############# Simpson class

setClass(Class="Simpson",  #Sets S4 class of door
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
  estimateLength = (length(object@estimate == 1))
  
  valuesLength= (length(object@x_values)== length(object@y_values))
  
  if(!estimateLength | !valuesLength){
    return("Simpson not valid")
  }
  
})

new("Simpson", x_values= x, y_values= y, estimate= 33.6)

setMethod("initialize", "Simpson", function(.Object, ...) { #initilize method 
  value = callNextMethod()
  validObject(value)
  return(value)
})

######## making input class

setClass(Class="input",  #Sets S4 class of door
         representation = representation(
           x_values = "numeric", # three slots as specified in problem set
           y_values = "numeric", 
           start_end_values = "numeric",
           test= "character"
         ),
         prototype = prototype(
           x_values = c(), #default values are empty 
           y_values = c(),
           start_end_values = c(),
           test= c()
         )
)

setValidity("input", function(object){ 
valuesLength= (length(object@x_values)== length(object@y_values))
  
  if(!valuesLength){
    return("Simpson not valid")
  }
  
})

new("input", x_values= x, y_values= y,start_end_values= StartToEnd, test= "Trap")

setMethod("initialize", "input", function(.Object, ...) { #initilize method 
  value = callNextMethod()
  validObject(value)
  return(value)
})



####### Making function

setGeneric("integrateIt", #sets generic function in S4
           function(x,y,Starting_Ending_Values,type) {
             standardGeneric("integrateIt")
           } )

setMethod("integrateIt", "input",
          function(x,y,Starting_Ending_Values,type){
  print("test")
            
          })














