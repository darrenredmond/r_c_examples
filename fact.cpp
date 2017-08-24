

extern "C" {
 
  void fact( int * x, int * fac){
  
  *fac = 1; //initialize factorial to 1 
  
  while( *x > 0 ){
    
      *fac *= (*x); //note brackets here not required 
      (*x)--; //decrement x
      
  } // end of while
 

  }// end of fact

} // end of extern "C" statement