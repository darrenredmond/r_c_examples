

library(Rcpp)
library(inline)
library(rbenchmark)


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


