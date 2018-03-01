###figuring out trapezoid

#equations is
#  y= -x^2 +5 

x<- c(1:4)
y<- c(1,4,9,16)

StartToEnd<- c(1:4)

trap<- function(x,y,StartToEnd){
  a= x[StartToEnd[1]]
  b= x[tail(StartToEnd, n=1)]
  n= as.numeric(length(StartToEnd))
  h=((b-a)/n)
  
  first_value= y[1]
  last_vlaue= tail(y, n=1)
  middle_vlaues= 2* y[2:(length(x)-1)]
  
  answer= (h/2)* sum((first_value+ middle_vlaues+ last_vlaue))
  
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
  middle_vlaues= y[2:(length(x)-1)]
  
  odd_middles= 4*middle_vlaues[seq(1,length(middle_vlaues),2)]
  
  even_middles= 2*middle_vlaues[seq(0,length(middle_vlaues),2)]

  
  answer= (h/3)* sum((first_value+ odd_middles+even_middles+ last_vlaue))
  
  return(answer)
}

simpsons(x,y,StartToEnd)
