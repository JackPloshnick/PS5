###figuring out trapezoid

#equations is
#  y= x^2

x.test<- c(1:9)
y.test<- c(11:20)

x<- c(1:5)
y<- c(11:20)

StartToEnd<- c(2:9)

trap<- function(x,y,StartToEnd){
  a= x[StartToEnd[1]]
  b= x[tail(StartToEnd, n=1)]
  n= as.numeric(length(StartToEnd)-1)
  h=((b-a)/n)
  
  first_value= y[head(StartToEnd, n=1)]
  last_vlaue= y[tail(StartToEnd, n=1)]
  middle_vlaues= 2* y[ (head(StartToEnd, n=1)+1):(tail(StartToEnd, n=1)-1)]
  
  answer= (h/2) * (first_value + last_vlaue + sum(middle_vlaues))
  
  if(length(x)!= length(y)){
    return("lengths are not equal")
  }
  
  else{
    return(answer)}
  
}

trap(x,y,StartToEnd)

#################

StartToEnd<- c(1:9)

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
    return("lengths are not equal")
  }
  
  else{
    return(answer)}
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

new("Trapezoid", x_values= x.test, y_values= y.test, estimate= 33.6)

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

yunk<-new("Simpson", x_values= x.test, y_values= y.test, estimate= 33.6)

setMethod("initialize", "Simpson", function(.Object, ...) { #initilize method 
  value = callNextMethod()
  validObject(value)
  return(value)
})





####### Making function

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





integrateIt(x.test,y.test,StartToEnd,"Simpson")

########### Print Method 


setMethod("print", "Trapezoid",
          function(x){
            print(x@estimate)})
         
print(yunk)

setMethod("print", "Simpson",
          function(x){
            print(x@estimate)})






