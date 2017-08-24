


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
