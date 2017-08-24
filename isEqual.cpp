extern "C" {

  void isEqual(double *x, double *y, int *isEq) {
    *isEq = *x == *y;
  }

}

