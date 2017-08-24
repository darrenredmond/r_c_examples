

extern "C" {
  
  void squareVec( double * x , int * len)
  {
  
    int n = *len; //length of R vector
    
    for( int i = 0; i < n ; i++ ) {
      
      x[ i ] = x[ i ] * x[ i ]; 
      
    } //end of for loop
    
  } //end of squareVec function
  
} //end of extern "C" statment