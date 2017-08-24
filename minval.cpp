


extern "C" {
  
 void minval( double * x , int * len, double * minval)
 {
   
  int n = *len; //length of R vector
  *minval = x[ 0 ]; //set minval to first element of x
    
  for( int i = 1; i < n ; i++ ) //iterate over 2nd element to nth element
  {
    
   if( x[ i ] < *minval ) //if element indexed by i is less than *minval 
   {
     
    *minval = x[ i ]; //set x[ i ] as current minimum value 
     
   } //end of if statement

  }  //end of for loop
  
 } //end of minval function 
  
} //end of extern "C" statement

