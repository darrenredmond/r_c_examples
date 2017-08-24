extern "C" {

  void plusTwoCpp(double *num) { // pointer argument
    *num = *num + 2; // adds 2 to the value pointed to by pointer num
  } // end of plusTwoCpp

} // end of extern "C" wrapper

