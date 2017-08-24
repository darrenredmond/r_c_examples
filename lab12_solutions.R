
library(Rcpp)
library(inline)
library(RcppArmadillo)
library(rbenchmark)

#QUESTION 1
#centering a matrix
centerRcpp <- cxxfunction( signature(X="numeric"),
                   body = '
                     mat M = as<mat>(X);
                     //compute mean of each col
                     rowvec mu = mean(M, 0);
                     //subtract mean vector from each row
                     M.each_row() -= mu;
                     return wrap(M);
                    ', 
                   include = 'using namespace arma;',
                   plugin = "RcppArmadillo")




data(iris)  #load iris data into R
names(iris) #extract names of variables in the data.frame
?iris  #get information on the iris dataset

#remove last column of the iris data.frame, 
#which contains the categorical variable Species
Y <-  as.matrix( iris[,-5] )
#call function and save centered data as centY
centY <- centerRcpp( Y )
#view first few rows 
head(centY)
#mean of each column should be 0 if centering succeeded
round( apply(centY,2,mean) , 10)





#QUESTION 2
#Outer product of two vectors
op <- cxxfunction( signature(x="numeric", y = "numeric"),
                    body = '
                     vec v1 = as<vec>(x);
                     vec v2 = as<vec>(y);
                     mat M = v1 * v2.t();
                     return wrap(M);
                    ', 
                    include = 'using namespace arma;',
                    plugin = "RcppArmadillo")

x <- rnorm(5)
y <- rnorm(3)
op(x,y)
