

#if you have not previously installed the rbenchmark package,
#you need to install it prior to use,
#using the install.packages() function 
install.packages("rbenchmark")

#prior to using rbenchmark during any session,
#you need to load it into R
#using the library() function
library(rbenchmark)






#PART 1: PROFILING R code

#measuring performance of R code
#identifying bottlenecks

#inefficient function to compute row means of a matrix
#input a matrix X and return a vector of its row means
#missing values are removed from 
#each row before computing mean

rowmean <- function( X ){
  
  #type checking
  if(!is.matrix(X))  stop("input is not a matrix") 
  if( ! ( is.numeric(X) || is.integer(X) ) )   
    stop("input should be of types integer or numeric")
  
  K <- nrow(X)  #number of rows
  vec <- rep(NA, K ) #to store row means
  
  for( i in 1:K ){
    
    x_i <- X[ i, ] #x_i is ith row of x
    xmis <- which( is.na( x_i ) ) #indices of missing values
    if( length(xmis) > 0 ){
      x_i <- x_i[ -xmis ] #removing missing values    
    } #end of if    
    vec[i] <- newsum( x_i )/ length( x_i ) #mean of row i
  } #end of for
  
  #return the vector of means
  vec
}


#function to compute sum of a vector

newsum <- function(x){
  
  xsum <- 0 #initialize sum to zero  
  #iterate over elements to cumulatively sum
  for(j in 1:length(x)){
    xsum <- xsum + x[ j ]
  }  
  #return the sum 
  xsum
}



X <- matrix( rnorm(500000), nrow = 10000)
dim(X)

#profile my code
setwd("~/Rcode")  #change working directory

Rprof("profile1.out" ) #send profiling to profile1.out
m <- rowmean( X )
Rprof(NULL)
summaryRprof("profile1.out")










#PART 2: BENCHMARKING

#compare performance of alternative implementations
#use the rbenchmark library

#install.packages("rbenchmark")
library(rbenchmark)


#implement rowmean function using R's built-in 
#sum function, instead of newsum() for summing rows
#call this implementation rowmean2

rowmean2 <- function( X ){
  
  #type checking
  if(!is.matrix(X))  stop("input is not a matrix") 
  if( ! ( is.numeric(X) || is.integer(X) ) )   
    stop("input should be of types integer or numeric")
  
  K <- nrow(X)  #number of rows
  vec <- rep(NA, K ) #to store row means
  
  for( i in 1:K ){
    
    x_i <- X[ i, ] #x_i is ith row of x
    xmis <- which( is.na( x_i ) ) #indices of missing values
    if( length(xmis) > 0 ){
      x_i <- x_i[ -xmis ] #removing missing values    
    } #end of if    
    vec[i] <- sum( x_i )/ length( x_i ) #mean of row i
  } #end of for
  
  #return the vector of means
  vec
}







#a tidier version uses the apply function in R,
#as well as R's built-in mean function
#Note: the built-in mean() has an option na.rm
#that allows you to remove missing values before calculating 
#the mean. Set na.rm = TRUE

rowmean3 <- function(X){

  #type checking
  if(!is.matrix(X))  stop("input is not a matrix") 
  if( ! ( is.numeric(X) || is.integer(X) ) )   
    stop("input should be of types integer or numeric")
  
  #return vector of row means
  apply(X, 1, function(row_j) mean(row_j, na.rm = TRUE))
}



#Now generate a large matrix of simulated data to pass to the 
#three completing functions during benchmarking
X <- matrix( rnorm(500000), nrow = 10000)

#benchmark the functions
benchmark(rowmean(X), rowmean2(X),rowmean3(X), order = "relative")





