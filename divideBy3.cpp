#include <R.h>

extern "C" {

  void divideBy3(int *x) {
    Rprintf("x divided by 3 is equal to %f \r\n ", static_cast < double >(*x)/3);
  }

}

