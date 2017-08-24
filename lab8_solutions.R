



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






#2 modifying the function to account for missing data


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

