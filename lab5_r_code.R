




#Q1  - load and execute squareVec.cpp

#change working directory to folder where compiled file is stored
setwd("path/to/file") 
list.files() #to check for .so of .dll file

dyn.load("squareVec.so") #on OS X
dyn.load("squareVec.dll") #on Windows

x <- c(0, 1, 2, 3)
.C("squareVec", x = as.numeric(x), len = as.integer(length(x)))

dyn.unload("squareVec.so") #on OS X
dyn.unload("squareVec.dll") #on Windows





#Q2- load and execute minval.cpp


#change working directory to folder where compiled file is stored
setwd("path/to/file") 
list.files() #to check for .so of .dll file

dyn.load("minval.so") #on OS X
dyn.load("minval.dll") #on Windows

x <- c( 1.0, 0.9, 0.6, 1.2 )
.C("minval", x = as.numeric( x ), len = as.integer(length(x)), 
   min = as.numeric(0))

dyn.unload("minval.so") #on OS X
dyn.unload("minval.dll") #on Windows







#Q3- load and execute fact.cpp


#change working directory to folder where compiled file is stored
setwd("path/to/file") 
list.files() #to check for .so of .dll file

dyn.load("fact.so") #on OS X
dyn.load("fact.dll") #on Windows

x <- 10  # integer input argument
.C("fact", as.integer(x), fact = as.integer(1))

#compare result with R's built-in factorial
factorial(x)

dyn.unload("fact.so") #on OS X
dyn.unload("fact.dll") #on Windows

