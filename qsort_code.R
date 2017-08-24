

# Simple implementation Quicksort algorithm in R.
# Quick sort algorithm:
# 1. Select a random value from the array.
# 2. Put all values less than the random in arrayLeft.
# 3. Put all values greater than the random in arrayRight.
# 4. If arrayLeft or arrayRight has more than 1 value, repeat the above steps on it.
# 5. The sorted result is arrayLeft, random, arrayRight.
#
# Kory Becker 9/4/2015
# http://primaryobjects.com/kory-becker
# Modified to handle missing values

quickSort <- function(x){

  recursiveSort <- function(arr){
   # Pick a number at random.
   mid <- sample(arr, 1)

   # Place-holders for left and right values.
   left <- c()
   right <- c()

   # Move all the smaller values to the left, bigger values to the right.
   lapply(arr[arr != mid], function(d) {
    if (d < mid) {
       left <<- c(left, d)
    }else{
       right <<- c(right, d)
    }
   } )

   if (length(left) > 1) {
     left <- recursiveSort(left)
   }
 
   if (length(right) > 1) {
     right <- recursiveSort(right)
   }

   # Finally, return the sorted values.
   c(left, mid, right)
 }  #end of recursiveSort

 if( all( is.na(x) ) ){  #if all values are missing
  warning("All input values are missing")
  return(x)
 }else{ #if there are non-missings
   mis <- which(is.na(x))
   if( length(mis) == 0 ){ # if no missings
     return(recursiveSort(x)) #return sorted
   }else{ #if there are some missings
     return( c(recursiveSort( x[-mis] ), x[mis]) )
   }
 }
}

set.seed(1001) #set seed of random number generator
x <- rnorm(10000) #generate some data
#set some randomly selected values to missing
x[ sample(10000,size = 100) ] <- NA 
quickSort(x) 


