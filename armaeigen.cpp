#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::vec getEigenValues(arma::mat M) {
    return arma::eig_sym(M);
}

/*** R
set.seed(42)
X <- matrix(rnorm(4*4), 4, 4)
X
Z <- X %*% t(X)
X
getEigenValues(Z)
*/

