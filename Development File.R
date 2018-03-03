
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd("C:\Users\jackp\Documents\GitHub\PS5") #This will need to be changed to match your directory

## This is run once when the package strcuture is first created


## At this point put the *.R files into the correcto directories and edit the DESCRIPTION file

## Let's look through the R directory in this order:

# squaresPack-package.r
# Squares.R
# addSquares.R
# subtractSquares.R
# AllSquares-class.R
# allSquares.R

# Now the NAMESPACE

## This can be run many times as the code is updates
current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)
check(current.code)


## Let's look at a function
x.test<- c(1,2,3,4,5)
y.test<- c(1,4,9,16,25)

StartToEnd<- c(1:5)

test<- trap(x.test, y.test, StartToEnd)
simpsons(x.test,y.test,StartToEnd)

S<- integrateIt(x.test,y.test,StartToEnd,"Simpson")
T<- integrateIt(x.test,y.test,StartToEnd,"Trap")
print(test)
