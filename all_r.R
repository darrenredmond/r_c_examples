


#LAB SHEET 1: solutions



library(rbenchmark) #load library for benchmarking



#measuring performance of R code
#identifying bottlenecks

#This is an inefficient function to count the 
#number of odd numbers in EACH ROW of a matrix
#Input is a matrix X, 
#Function returns a vector of counts

#The function does some type-checking
#tests whether input is a matrix, 
#then whether it is of integer type

countOdds <- function( X ){
  
 #type checking
 if( !is.matrix(X) )  stop("input is not a matrix") 
 if( !is.integer(X) ) stop("input should be of type integer")
  
 N <- nrow(X)  #number of rows of X

 #set up a vector to store counts for each row of X
 #initialize to zero
 numOdds <- rep(0, N ) 
  
 for( i in 1:N ){  #for each row i in X
    #call function to count odd numbers in row i 
    numOdds[i] <- vecOdds( X[ i, ] )
 } #end of for
  
  #return the vector of means
  numOdds
}


#count the odd numbers in a vector y
#(note: the vector will actually be a row of X)
#count the number of odd numbers in a vector y
vecOdds <- function(y){ 
  
  K <- length(y) #length of the vectors (row)
  #keep count of number of odd numbers in y
  sumOdds <- 0   #initialize counter to 0 (no odd numbers)
  
  for(j in 1:K){ #for each element of the vector y
    sumOdds <- sumOdds + isOdd( y[j] ) #update sumOdds
  }
  return(sumOdds)
} #end of vecOdds()




#function that checks whether its input is an odd number
#returns 1 if input is odd and 0 otherwise
#note the modulus operator %% that returns the remainder from
#the division of two numbers
isOdd <- function(num){
  if( num %% 2 ){
    return(1)
  }else{
    return(0)
  }
} #end of isOdd



#CREATE SOME DATA TO TEST THE FUNCTION
#generate data from a binomial distribution
simdata <- rbinom(n=10000000, size=30, prob = 0.5)

#what the simulated data look like
barplot(table(simdata)) 

#arrange data into a matrix with 100000 rows
X <- matrix(simdata, nrow= 100000)
dim(X) #dimensionts of matrix


#profile my code

Rprof("profile1.out" ) #send profiling to profile1.out
countOdds(X)
Rprof(NULL)  #end profiling
summaryRprof("profile1.out")  #summary of profiling



#my profiling shows total elapsed time of 11/12 minutes
#around 35% of time was spent in the rowOdd() function, 
#while a huge 50% (approx) of the time was spent in the isOdd function
#this function can definitely be improved on



#IMPROVED CODE

#use apply() function instead of a for loop over the rows


newCountOdds <- function(X){

  #type checking
  if( !is.matrix(X) )  stop("input is not a matrix") 
  if( !is.integer(X) ) stop("input should be of type integer")
  #count number of odd numbers in each row
  counts <- apply(X, 1, function( row_i ){  sum( row_i %% 2) } )
  return(counts) #return the vector of counts
}



benchmark(newCountOdds(X), countOdds(X), order = "relative")


# LAB 3 - Extern C - code found in plusTwoCpp.cpp
# code compiled with -
# R CMD SHLIB plusTwoCpp.cpp
setwd("~/Downloads/r_exam")
dyn.load("plusTwoCpp.so")
.C( "plusTwoCpp", as.numeric(3.2) )
dyn.unload("plusTwoCpp.so")

dyn.load("cube.so")
.C( "cube", as.numeric(3) )

# examples of calls to cube ()
.C("cube", as.numeric(0.3))
.C("cube", as.numeric(2))
.C("cube", as.numeric( -2))

dyn.unload("cube.so")

dyn.load("isEqual.so") # load the compiled function (for OS X)

# call the function isEqual through the .C interface with equal arguments
.C("isEqual", x = as.numeric(1.1), y = as.numeric(1.1), result = TRUE)
# with non equal arguments
.C("isEqual", x = as.numeric(1.2), y = as.numeric(1.1), result = TRUE)


# write a wrapper function in R that takes two numeric input arguments
# returns a single value , TRUE or FALSE ,
# depending on whether its input arguments are equal or not
isEqual <- function(x, y) {
  if (!(is.numeric(x) & is.numeric(y))) {
    stop("input arguments must be numeric")
  }
  if ((length(x) > 1) | (length(y) > 1)) {
    stop("input arguments must be scalars")
  }
  return (.C("isEqual", x = x, y = y, out = FALSE)$out)
}

# call C++ through the wrapper function
isEqual(2.7, 2.7)
isEqual(2.9, 2.7)

dyn.unload("isEqual.so") # unload compiled function (OS X)
#dyn.unload("isEqual.dll") # unload compiled function (Windows)

dyn.load("divideBy3.so") # unload the compiled function ( for OS X)
#dyn.load("divideBy3.dll") # unload the compiled function (for Windows )

.C("divideBy3", x = as.numeric(3))
.C("divideBy3", x = as.numeric(4))

# write a wrapper function
divideBy3 <- function(x) {
  # check that x is an integer scalar
  #if not, break out of function using stop ()
  if (!is.integer(x) | (length(x) != 1)) {
    stop("x must be an integer scalar")
  }
  result <- .C("divideBy3", as.integer(x)) # call compiled C++ function
}

# call the C++ function through the R wrapper
divideBy3(as.integer(3))
divideBy3(as.integer(4))

dyn.unload("divideBy3.so") # unload compiled function (OS X)
#dyn.unload("divideBy3.dll") # unload compiled function (Windows)

#Q1  - load and execute squareVec.cpp

#change working directory to folder where compiled file is stored
list.files() #to check for .so of .dll file

dyn.load("squareVec.so") #on OS X
#dyn.load("squareVec.dll") #on Windows

x <- c(0, 1, 2, 3)
.C("squareVec", x = as.numeric(x), len = as.integer(length(x)))

dyn.unload("squareVec.so") #on OS X
#dyn.unload("squareVec.dll") #on Windows

#Q2- load and execute minval.cpp
#change working directory to folder where compiled file is stored

dyn.load("minval.so") #on OS X
#dyn.load("minval.dll") #on Windows

x <- c( 1.0, 0.9, 0.6, 1.2 )
.C("minval", x = as.numeric( x ), len = as.integer(length(x)), 
   min = as.numeric(0))

dyn.unload("minval.so") #on OS X
#dyn.unload("minval.dll") #on Windows

#Q3- load and execute fact.cpp
#change working directory to folder where compiled file is stored

dyn.load("fact.so") #on OS X
#dyn.load("fact.dll") #on Windows

x <- 10  # integer input argument
.C("fact", as.integer(x), fact = as.integer(1))

#compare result with R's built-in factorial
factorial(x)

benchmark(factorial(x), .C("fact", as.integer(x), fact = as.integer(1)), order = "relative")

dyn.unload("fact.so") #on OS X
#dyn.unload("fact.dll") #on Windows


library(Rcpp)
library(inline)

# the function body is defined in this R character string
body_divideInt <- '
  int val1 = as<int>(x1);
  int val2 = as<int>(x2);
  return wrap (static_cast<double>(val1)/val2);
'
# compile, link and load the C++ function
divideInt <- cxxfunction(
  signature (x1="integer", x2="integer"),
  body = body_divideInt,
  plugin = "Rcpp"
)

# call the compiled C++ function from R
divideInt(as.integer(4), as.integer(3))
divideInt(as.integer(8), as.integer(2))

#A C++ equivalent of the which.min function
library(Rcpp)
library(inline)

#create the function body as an R character string
body_whichMinCpp <- '
IntegerVector xx(x); //convert to IntegerVector object
int minval = xx[ 0 ]; //current minimum set to first element
int min_index = 1; //current minimum is first element

for( int i = 1 ; i < xx.size(); i++ )
{
  //check if element indexed by i is 
  //less than the current minimum 
  if( xx[ i ] < minval )
  {
  //update minimum value
  minval = xx[ i ];
  //update position of minimum element
  min_index = i+1; //because element indexed by i is the (i+1)th element
  }
} //end of for loop
  
return wrap( min_index );
'
  
#compile, link, load
whichMinCpp <- cxxfunction(
  signature(x = "integer"),
  body = body_whichMinCpp,
  plugin = "Rcpp"
)

#create some data to test it on
x <- sample(1:20, size = 10, replace = TRUE)

#call the compiled C++ function
whichMinCpp( x )

#EXERCISE: OPTIMIZE THE BUBBLE SORT ALGORITHM

#ORIGINAL BUBBLE SORT

#(using clone() to dupicate input vector)
#in this example, the clone() function is used to copy the data from R,
#so that sorting of the vector does not modify the original vector passed by R
body_bubblesort2 <- '

IntegerVector xx = clone(x); //use of clone() 
int n = xx.size(); //no. of elements
int temp; //temporary storage of swap value

for( int k = 1; k <= n - 1; k++ ){ //for pass k

//loop over pairs of elements
for( int i = 0; i < n - 1; i ++ ){
if( xx[ i ] > xx[ i+1 ] ){
temp = xx[ i + 1 ];
xx[ i + 1 ] = xx[ i ];
xx[ i ] = temp;
} 

} //end of loop over array pairs
} //end of loop over passes
return(wrap(xx));
'

bubblesort2 <- cxxfunction(signature( x = "integer" ),
                           body = body_bubblesort2,
                           plugin = "Rcpp")

x2 <- as.integer( sample(1:100, size = 100, replace = FALSE) )
bubblesort2(x2) #sorts x2
x2 #x2 is not sorted

#in this example, the clone() function is used to copy the data from R,
#so that sorting of the vector does not modify the original vector passed by R
body_bubblesortOpt <- '

IntegerVector xx = clone(x); //use of clone() 
int n = xx.size(); //no. of elements
int temp; //temporary storage of swap value

for( int k = 1; k <= n - 1 ; k++ ){ //for pass k

//loop over pairs of elements
for( int i = 0; i < n - k ; i ++ ){ //modified i < n-1 to i < n-k to avoid passing over elements that are already sorted
if( xx[ i ] > xx[ i+1 ] ){
temp = xx[ i + 1 ];
xx[ i + 1 ] = xx[ i ];
xx[ i ] = temp;
}

} //end of loop over array pairs
} //end of loop over passes
return(wrap(xx));
'

bubblesortOpt <- cxxfunction(signature( x = "integer" ),
                             body = body_bubblesortOpt,
                             plugin = "Rcpp")

x <- as.integer( sample(1:100, size = 100, replace = FALSE) )
sorted_x <- bubblesortOpt(x) #sorts x
sorted_x #sorted_x is sorted
x #x is not sorted

library(Rcpp)
library(inline)

#1 WELFORD'S ALGORITHM FOR COMPUTING VARIANCE

body_varCpp <- '
NumericVector vec(x);
int n = vec.size(); // length of vec
double M_k = vec[ 0 ]; // mean of first k elements
double S_k = 0;  // sum of squared differences of first k elements
double M_prev;  // mean of first (k-1) elements

//if there is only one element, return variance = 0
if( n < 2){ return wrap(0); }  

//iterate over 2nd to nth element 
for( int k = 2; k <=n; k++ ){
M_prev = M_k ; //mean of first (k-1) elements
M_k += ( vec[ k-1 ] - M_k) / k ; //update M_k: mean of first k elements

//update S_k sum of squared differences of first k elements
S_k += ( vec[ k-1 ] - M_prev) * ( vec[ k-1 ] - M_k ); 
}

return wrap(  S_k / (n-1) );
'

#compile the above C++ function
varCpp <- cxxfunction( signature( x = "numeric"),
                       body = body_varCpp,
                       plugin = "Rcpp")

#generate some data
x <- rnorm(1000)

#call compiled code
varCpp(x)

#built-in variance function
var(x)

#benchmark
benchmark( var(x), varCpp(x), order = "relative")[,1:4]

body_varCpp2 <- '

NumericVector vec(x);
int len = vec.size(); // total length of vec
int first = 0; //to store index of first non-missing element
double M_k, S_k = 0;  // mean, sum of squared differences of first k elements
double M_prev;  // mean of first (k-1) elements
int n=1; //to count non-missing values

while( !R_finite( vec[ first ] ) && first < len){ //while the value is missing
first++;
}

M_k = vec[first]; //mean of first k elements, initialize to first non-missing element
Rprintf("M_k first value is %f   ", M_k);

if(first < len){ //if there is at least one non-missing value

for( int k = first+2 ; k <= len; k++ ){ //loop to find non-missing values

if( R_FINITE( vec[k-1] )){ //if the next element is not missing, update M_k, S_k
n++; //add to number of non-missing values
M_prev = M_k ; //mean of previous non-missing elements  
M_k += ( vec[k-1] - M_k) / n ;   //update mean
S_k += ( vec[k-1] - M_prev) * ( vec[k-1] - M_k );  //update sum of squared differences      
}   

}
return wrap(  S_k / (n-1) );
}else{
return wrap( 0 );
}
'

#compile the above C++ function
varCpp2 <- cxxfunction( signature( x = "numeric"),
                        body = body_varCpp2,
                        plugin = "Rcpp")


set.seed(1001)
x <- rnorm(10)
x[1:3] <- NA
var(x, na.rm=TRUE)
varCpp(x)
varCpp2(x)


set.seed(1001)
x <- rnorm(10)
x[c(2,4,6)] <- NA
var(x, na.rm=TRUE)
varCpp(x)
varCpp2(x)

#UNIVARIATE GRADIENT DESCENT ALGORITHM
#OBJECTIVE: MINIMIZE THE FUNCTION 


#FIRST, DEFINE THE FUNCTION f(theta)
objfun <- function(theta){
  return(1 + 3*(theta + 3)^2 )
}

#DEFINE A FUNCTION THAT CALCULATES THE DERIVATIVE f'(theta) 
# AT INPUT VALUE theta
deriv <- function(theta)
{
  return( 6*(theta + 3 ) )
}





#PLOT THE FUNCTION WE ARE SEEKING TO MINIMIZE OVER THE RANGE [-15, 15]
theta_seq <- seq(-20,15, len = 1000)
plot(theta_seq, objfun(theta_seq), type = "l", 
     ylab = "f(theta)", xlab = "theta")

points(10, objfun(10), col = "red")


tol <- 0.0000001 #CONVERGENCE THRESHOLD
alpha <- 0.01  #LEARNING RATE
theta0 <- 10  #SELECT INITIAL GUESS FOR THETA
newval <- objfun( theta0 ) #inital value of f(theta)
points(theta0, newval, col = "red") #add current theta to plot
rel_ch <- 1 #(arbitrary) init. val for relative change in objective function
j <- 1 #iteration counter
theta <- c() #vector to store theta at each iteration
theta[j] <- theta0 #theta[j] stores current value of theta

#update theta while relative change in f(theta) is greater than tol
while( rel_ch > tol ) 
{
  j <- j+1; #increment j
  #update theta
  #set theta_new =  theta_previous - ( learning rate )* f'(theta_previous)
  theta[j] <- theta[j-1] - alpha * deriv(theta[ j-1 ])
  
  #test relative absolute change in target function
  oldval <- newval #store f(theta_previous) 
  newval <- objfun(theta[j]) #calculate f(theta_new)
  points(theta[j], newval, col = "red") #add new theta to plot
  Sys.sleep(0.1) #pause algorithm to give you time to see dot appear on plot
  #calculate relative change f(theta) from previous iteration to current iteration
  rel_ch <- abs(  ( newval - oldval ) / oldval ) #use to test convergence
}








#R version EXECUTED WITHOUT A PLOT (NON SLOW MOTION VERSION)

objfun <- function(theta){
  return(0.1 + 0.1*theta + (theta^2)/( 0.1 + theta^2 ))
}


deriv <- function(theta)
{
  return( 0.1 + ( 0.2*theta + 4*theta^3 ) / (0.1 + theta)^2)
}



gdes <- function( theta0  , tol = 0.00000001, alpha = 0.01 , )
{
  
  newval <- objfun( theta0 ) #inital value of target function
  rel_ch <- 1 #to store relative change in objective function
  j <- 1 #iteration counter
  theta <- c(theta0) #vector to store parameter
  
  while( rel_ch > tol )
  {
    j <- j+1; #increment counter
    theta[j] <- theta[j-1] - alpha * deriv(theta[ j-1 ])
    
    #test relative absolute change in target function
    oldval <- newval
    newval <- objfun(theta[j])
    rel_ch <- abs(  ( newval - oldval ) / oldval )
  } #end of while
  
  return( list( theta = theta[j], thetavec = theta, min_f = newval, niter = j ) )
} #end of gdes function 


opt <- gdes(-0.5) 
opt$theta
opt$min_f






#QUESTION 1 solution

#C++ implementation of univariate gradient descent
#define target function and its derivative
#also include the iostream library
#and using std::endl


incl <- '
#include <iostream>
using std::endl;

double obj( double theta )
{
  return( 0.1 + 0.1*theta + pow(theta,2) /( 0.1 + pow(theta, 2) ) );
}

double deriv( double theta )
{
  return( 0.1 + ( 0.2*theta + 4*pow(theta, 3) ) / pow(0.1 + theta, 2) );
}
'

body_gdC <- '
double val0 = as<double>(theta0);
double tol = as<double>(tolerance);
double learnrate = as<double>(alpha);

double theta = val0; //current value of theta
double theta_prev; //stores previous value of theta
double rel_ch = 1.0;
double ftheta = obj( theta );  //stores value of target fn at current theta
//Rcout << "Objective function = " << ftheta << std::endl ;
double ftheta_prev; //to store value of target function at previous theta
int j = 0;

while( rel_ch > tol )
{
  j++;
  theta_prev = theta;
  ftheta_prev = ftheta;
  theta = theta_prev - learnrate * deriv( theta_prev );
  ftheta = obj( theta );
  //Rcout << "f(theta) = " << ftheta   << std::endl;
  rel_ch =  fabs( ( ftheta - ftheta_prev ) / ftheta_prev ) ;
}

return wrap( List::create(
_["theta"] = theta,
_["ftheta"] = obj( val0 ) 
));
'

gdC <- cxxfunction( signature( theta0 = "numeric" , tolerance = "numeric", alpha = "numeric"),
                    body = body_gdC,
                    includes = incl,
                    plugin = "Rcpp")


gdC( -0.5 , 0.00000000001, 0.01)

#QUESTION 2 solution

#same as question 1 solution but with Bold Driver method implemented
#adapts learning rate alpha at each iteration
incl <- '
#include <iostream>
using std::endl;

double obj( double theta )
{
  return( 0.1 + 0.1*theta + pow(theta,2) /( 0.1 + pow(theta, 2) ) );
}

double deriv( double theta )
{
  return( 0.1 + ( 0.2*theta + 4*pow(theta, 3) ) / pow(0.1 + theta, 2) );
}
'

body_gdC_2 <- '
double val0 = as<double>(theta0);
double tol = as<double>(tolerance);
double learnrate = as<double>(alpha);

double theta = val0; //current value of theta
double theta_prev; //stores previous value of theta
double rel_ch = 1.0;
double ftheta = obj( theta );  //stores value of target fn at current theta
Rcout << "Objective function = " << ftheta << std::endl ;
double ftheta_prev; //to store value of target function at previous theta
int j = 0;

while( rel_ch > tol )
{
  j++;
  theta_prev = theta;
  ftheta_prev = ftheta;
  theta = theta_prev - learnrate * deriv( theta_prev );
  ftheta = obj( theta );
  if( ftheta - ftheta_prev <= 0 ) //if target decreased or remained the same
  {
  learnrate *= 1.05; //increase learning rate by 5%
  }else{ //if target function increased (BAD)!
  learnrate *= 0.5; //decrease learning rate by 50%
  }
  Rcout << "learning rate = " << learnrate  << std::endl;
  rel_ch =  fabs( ( ftheta - ftheta_prev ) / ftheta_prev ) ;
}

return wrap( List::create(
_["theta"] = theta,
_["ftheta"] = obj( val0 ) 
));
'

gdC_2 <- cxxfunction( signature( theta0 = "numeric" , tolerance = "numeric", alpha = "numeric"),
                      body = body_gdC_2,
                      includes = incl,
                      plugin = "Rcpp")


gdC_2( -0.5 , 0.00000000001, 0.01)

#QUESTION 3 function to return mean and standard deviation
#part 1 : does not handle missing values

#uses Rcpp sugar functions min(), max(), mean(), sd() 

body_summaryC <- '
NumericVector xx(x);
return wrap( NumericVector::create( 
_["min"] = min(xx),
_["max"] = max(xx),
_["mean"] = mean(xx),
_["sd"] = sd(xx)
) );
'

summaryC <- cxxfunction(signature(x = "numeric"),
                        body = body_summaryC,
                        plugin = "Rcpp")

#generate some data to test the function on
x <- rnorm(20)
summaryC(x) #call C++

#now set first 3 elements to missing
x[1:3] <- NA

#how does summaryC() handle missings?
summaryC(x) #call C++

#handles missing values
#second input argument to indicate whether missing values should be removed
#if true, remove missing values
#otherwise, do not remove
#USE vector subsetting to remove missing values na.rm == TRUE
body_summaryC_2 <- '
NumericVector xx(x);
int remNA = as<int>(na_rm);
if(remNA == 1){ //if true
xx = xx[!is_na(xx)];
}
return wrap( NumericVector::create( 
_["min"] = min(xx),
_["max"] = max(xx),
_["mean"] = mean(xx),
_["sd"] = sd(xx)
) );
'

summaryC_2 <- cxxfunction(signature(x = "numeric", na_rm = "logical"),
                          body = body_summaryC_2,
                          plugin = "Rcpp")

#generate some data to test the function on
x <- rnorm(20)
summaryC_2(x, na_rm = TRUE) #call C++

#now set first 3 elements to missing
x[1:3] <- NA

#how does summaryC() handle missings?
summaryC_2(x, na_rm = TRUE) #remove missings
summaryC_2(x, na_rm = FALSE) #don't remove missings

#QUESTION 2
#count number of missing values in a vector

body_countMiss <- '
NumericVector xx(x);
LogicalVector y = is_na(xx);
int result = std::count(y.begin(),y.end(), true);
return wrap(result);
'

countMiss <- cxxfunction( signature(x = "numeric"),
                          body = body_countMiss,
                          plugin= "Rcpp")

x <- rnorm(100)
#set 10 values from x to zero
x[sample(1:100, size = 10, replace = FALSE)] <- NA
countMiss(x)

#QUESTION 3
#merge two sorted vectors into a longer sorted vector

body_mergeTwo <- '
NumericVector xx = clone(x);
NumericVector yy = clone(y);
std::sort(xx.begin(), xx.end());
std::sort(yy.begin(), yy.end());
NumericVector out = NumericVector( xx.size() + yy.size());
std::merge(xx.begin(), xx.end(), yy.begin(), yy.end(), out.begin());
return wrap(out);
'

mergeTwo <- cxxfunction( signature(
  x = "numeric", y = "numeric"),
  body = body_mergeTwo,
  plugin= "Rcpp")

x <- rnorm(10)
y <- rnorm(10)
x
y
mergeTwo(x,y)

sourceCpp('median.cpp')
median_rcpp(1:10)
median_rcpp(1:9)

set.seed(123)
z <- rnorm(1000000)
benchmark(median_rcpp(z), median(z), order="relative")[,1:4]

# benchmark mad_rcpp and mad
benchmark(mad_rcpp(z), mad(z), order="relative")[,1:4]

sigma <- matrix(c(1, 0.9, -0.3, 0.9, 1, -0.4, -0.3, -0.4, 1), ncol = 3)
mu <- c(10, 5, -3)

sourceCpp('mvrnormarma.cpp')
cor(mvrnormArma(100, mu, sigma))

### naive implementation in R
mvrnormR <- function(n, mu, sigma) {
  ncols <- ncol(sigma)
  mu <- rep(mu, each = n) ## not obliged to use a matrix (recycling)
  mu + matrix(rnorm(n * ncols), ncol = ncols) %*% chol(sigma)
}

### checking variance
set.seed(123)
cor(mvrnormR(100, mu,  sigma))

require(MASS)
cor(MASS::mvrnorm(100, mu, sigma))

benchmark(mvrnormR(1e4, mu, sigma),
          MASS::mvrnorm(1e4, mu, sigma),
          mvrnormArma(1e4, mu, sigma),
          columns = c('test', 'replications', 'relative', 'elapsed'),
          order = 'elapsed')

## checking means
colMeans(mvrnormR(100, mu, sigma))
colMeans(MASS::mvrnorm(100, mu, sigma))
colMeans(mvrnormArma(100, mu, sigma))

library(RcppArmadillo)

#QUESTION 1
#centering a matrix
centerRcpp <- cxxfunction( signature(X="numeric"),
                           body = '
                           mat M = as<mat>(X);
                           //compute mean of each col
                           rowvec mu = mean(M, 0);
                           //subtract mean vector from each row
                           M.each_row() -= mu;
                           return wrap(M);
                           ', 
                           include = 'using namespace arma;',
                           plugin = "RcppArmadillo")

data(iris)  #load iris data into R
names(iris) #extract names of variables in the data.frame
?iris  #get information on the iris dataset

#remove last column of the iris data.frame, 
#which contains the categorical variable Species
Y <-  as.matrix( iris[,-5] )
#call function and save centered data as centY
centY <- centerRcpp( Y )
#view first few rows 
head(centY)
#mean of each column should be 0 if centering succeeded
round( apply(centY,2,mean) , 10)

#QUESTION 2
#Outer product of two vectors
op <- cxxfunction( signature(x="numeric", y = "numeric"),
                   body = '
                   vec v1 = as<vec>(x);
                   vec v2 = as<vec>(y);
                   mat M = v1 * v2.t();
                   return wrap(M);
                   ', 
                   include = 'using namespace arma;',
                   plugin = "RcppArmadillo")

x <- rnorm(5)
y <- rnorm(3)
op(x,y)

#x = input vector
#k = period of moving average
movingAverage <- function (x , k) {
  n <- length (x) # length of input vector
  nMA <- n - k + 1 #the number of elements in the moving average
  xnew <- rep (NA , nMA) #to store moving average
  i <- 1 # counter variable
  #to calculate moving average , will calculate average of
  # the next k elements starting from position i
  while (i <= nMA){ # until the last moving average
    # calculate average for k values starting from element i
    xnew [i] <- mean (x[(i:(i+k-1))])
    i <- i+1
  }
  xnew # return moving average of period k
}
# call the function
x <- c(3.5 , 3.2 , 2.9 , 3.1 , 2.9 , 2.8 , 3.0 , 2.7)
movingAverage(x, k=1)
movingAverage(x, k=5)

sourceCpp('armaeigen.cpp')

data(iris) # load iris data into R
names(iris) # extract names of variables in the data . frame
?iris # get information on the iris dataset

# remove last column of the iris data .frame ,
# which contains the categorical variable Species
Y <- as.matrix(iris[ , -5])

Y

closest <- function(data) {
  nRowsDf <- nrow(data)
  values = c()
  js = c()
  is = c()
  for(i in 1:nRowsDf) {
    for(j in i:nRowsDf) {
      if (i != j) {
        values = c(values, (data[[i,1]]-data[[j,1]])**2 + (data[[i,2]]-data[[j,2]])**2 + (data[[i,3]]-data[[j,3]])**2 + (data[[i,4]]-data[[j,4]])**2)
        is = c(is, i)
        js = c(js, j)
      }
    }
  }
  data.frame(is, js, values) 
}
head(Y)
my_data <- closest(head(Y))
head(my_data[order(c(my_data[,3], my_data[,1]), decreasing = FALSE),])

my_data <- closest(Y)
head(my_data[order(c(my_data[,3], my_data[,1]), decreasing = FALSE),])

head(my_data[order(c(my_data[,3], my_data[,1]), decreasing = FALSE),])

head(my_data[ order(my_data[,1], my_data[,3], my_data[,2]), ])

head(my_data[order(my_data),])

#A first example with RcppArmadillo

body_firstfun <- '
arma::mat A = arma::randn<arma::mat>(4, 5);
arma::mat B = arma::randn<arma::mat>(4, 5);

arma::mat C = A * arma::trans(B);
C.print("C:");

return R_NilValue;
'

firstfun <- cxxfunction(signature(),
                        body = body_firstfun,
                        plugin = "RcppArmadillo")

result <- firstfun()

result

body_firstfun2 <- '
 mat A = randn<mat>(4, 5);
 mat B = randn<mat>(4, 5);

mat C = A * trans(B);
C.print("C:");

return R_NilValue;
'

firstfun2 <- cxxfunction(signature(),
                         body = body_firstfun2,
                         includes = 'using namespace arma;',
                         plugin = "RcppArmadillo")

result <- firstfun2()

body_ex1 <- '
vec v = as<vec>(x);
uvec idx = as<uvec>(ind);
vec vsub = v.elem(idx - 1);
return wrap( vsub );
'
f1 <- cxxfunction(
  signature(x = "numeric", ind = "integer" ),
  body = body_ex1,
  includes = 'using namespace arma;',
  plugin = "RcppArmadillo")
x <- rnorm(5)
x
x_index <- c(1,3)
f1(x, x_index)

# create functions which can be included in the cxxfunction call to simplify and allow code reuse.
incl_closest <- '
#include <iostream>
using std::endl;
using namespace arma;

double euclid(int i, int j, mat m) {
  return (pow(pow(m(i, 0)-m(j, 0), 2) + pow(m(i, 1)-m(j, 1), 2) + pow(m(i, 2)-m(j, 2), 2) + pow(m(i, 3)-m(j, 3), 2), 0.5));
}

vec closest(mat m) {
  vec v(3);
  double value = -0.1;
  double smallest = -0.1;
  for (int i = 0; i < m.n_rows; i++) {
    for (int j = i+1; j < m.n_rows; j++) {
      value = euclid(i, j, m);
      if (smallest < 0 || value < smallest) {
        smallest = value;
        v(0) = i+1;
        v(1) = j+1;
        v(2) = value;
      }
    }
  }
  return v;
}
'

body_closest <- '
mat m = as<mat>(x);
return wrap(closest(m));
'
closestpairCpp <- cxxfunction(
  signature(x = "numeric"),
  body = body_closest,
  includes = incl_closest,
  plugin = "RcppArmadillo")

data(iris) # load iris data into R
names(iris) # extract names of variables in the data . frame
?iris # get information on the iris dataset

# remove last column of the iris data .frame ,
# which contains the categorical variable Species
Y <- as.matrix(iris[ , -5])

closestpairCpp(Y)[[3,1]]

closestpairCpp(head(Y))[[3,1]]

body_unique <- 'return wrap(closest(as<mat>(x))(2) > 0);'

body_unique <- '
  mat m = as<mat>(x);
  vec v = closest(m);
  bool isUnique = v(2) > 0;
  return wrap(isUnique);
'
isUnique <- cxxfunction(
  signature(x = "numeric"),
  body = body_unique,
  includes = incl_closest,
  plugin = "RcppArmadillo")

head(Y)
uniqueCpp(head(Y))
uniqueCpp(Y)

isUnique (Y) #are all rows unique ?
isUnique (Y[1:10 ,]) # are the first 10 rows unique ?
