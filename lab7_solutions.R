
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








#SOLUTION TO THE BUBBLESORT PROBLEM



library(Rcpp)
library(inline)
library(rbenchmark)



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









#MODIFICATION 1:Exercise: Modify the bubblesort algorithm so that on pass k, it does not iterate through the top k-1 elements of the array. 

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






#MODIFICATION 2: adding on from modification 1, the following function tests at each pass whether a swap was made during the pass
#if a swap was NOT made, the array is already sorted, so no further passes are required...finish the algorithm early

#in this example, the clone() function is used to copy the data from R,
#so that sorting of the vector does not modify the original vector passed by R
body_bubblesortOpt2 <- '

IntegerVector xx = clone(x); //use of clone() 
int n = xx.size(); //no. of elements
int temp; //temporary storage of swap value
int wasSwap = 1; //to test whether a swap was made on pass k
//set wasSwap to 0 if no swap was made, 1 otherwise

for( int k = 1; (k <= n - 1) && (wasSwap == 1 ); k++ ){ //for pass k, continue if k <= n-1 AND a swap was made on previous pass
 wasSwap = 0; //so far, no swap has been made on this pass, so set to zero

 //loop over pairs of elements
 for( int i = 0; i < n - k ; i ++ ){ //modified i < n-1 to i < n-k to avoid passing over elements that are already sorted
   if( xx[ i ] > xx[ i+1 ] ){
    temp = xx[ i + 1 ];
    xx[ i + 1 ] = xx[ i ];
    xx[ i ] = temp;
    wasSwap = 1;  // indicate that a swap was made
   }

 } //end of loop over array pairs
} //end of loop over passes
return(wrap(xx));
'

bubblesortOpt2 <- cxxfunction(signature( x = "integer" ),
                              body = body_bubblesortOpt2,
                              plugin = "Rcpp")

x <- as.integer( sample(1:100, size = 100, replace = FALSE) )
sorted_x <- bubblesortOpt2(x) #sorts x
sorted_x #sorted_x is sorted
x #x is not sorted



#compare performance of the bubblesort functions
#bubblesortOpt2 is a clear winner

x <- as.integer( sample(1:100, size = 100, replace = FALSE) )
benchmark(bubblesort2(x), bubblesortOpt(x), bubblesortOpt2(x), 
          order = "relative",
          replications = 1000)








