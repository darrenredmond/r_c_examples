
library(Rcpp)
library(inline)



#QUESTION 2
#count number of missing values in a vector

body_countMiss <- '
 NumericVector xx(x);
 LogicalVector y = is_na(xx);
 int result = std::count(y.begin(),y.end(), true);
 return wrap(result);
'

countMiss <- cxxfunction( signature(x = "numeric"),
                          body = body_countMiss,
                          plugin= "Rcpp")

x <- rnorm(100)
#set 10 values from x to zero
x[sample(1:100, size = 10, replace = FALSE)] <- NA
countMiss(x)






#QUESTION 3
#merge two sorted vectors into a longer sorted vector


body_mergeTwo <- '
 NumericVector xx = clone(x);
 NumericVector yy = clone(y);
 std::sort(xx.begin(), xx.end());
 std::sort(yy.begin(), yy.end());
 NumericVector out = NumericVector( xx.size() + yy.size());
 std::merge(xx.begin(), xx.end(), yy.begin(), yy.end(), out.begin());
 return wrap(out);
'

mergeTwo <- cxxfunction( signature(
                          x = "numeric", y = "numeric"),
                          body = body_mergeTwo,
                          plugin= "Rcpp")


x <- rnorm(10)
y <- rnorm(10)
x
y
mergeTwo(x,y)




