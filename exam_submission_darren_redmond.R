

library(Rcpp)
library(inline)

# QUESTION 1

incl_random_walk = '
#include<iostream>
#include<cstdlib>

using namespace std;

// constant for where visited
const int VISITED = 1;

bool locate() {

int visited = VISITED;
// create a board size
int N = 1000;
// have offset to prevent setting negative indexing
int OFFSET = 500;
// probably do not need to track the board but just in case
int board[N][N];

// the location to find - must take into account the offset.
int xLocation = OFFSET - 10;
int yLocation = OFFSET + 30;

int curX = 0 + OFFSET;
int curY = 0 + OFFSET;

// keep track of everwhere that was visited in case we want to draw the grid
board[curX][curY] = visited;

srand(1234);

int dir;
int totalSteps = 0;
int numSteps = 1000;
bool debug = false;
bool found = false;
// while less than max steps walked - 1000, and have not found the place then try again
while (totalSteps < numSteps && !found) {
dir = rand() %4 +1;
if (debug) {
cout << dir;
}
// going east
if (dir == 1) {
board[curX][++curY] = visited;
// going west
} else if(dir == 2) {
board[curX][--curY] = visited;
// going north
} else if(dir == 3) {
board[--curX][curY] = visited;
// going south - make these const.
} else if(dir == 4) {
board[++curX][curY] = visited;
}
// check to see if we have found the location
if (curX == xLocation && curY == yLocation) {
found = true;
}
totalSteps++;
   }
   if (found) {
cout << "found";
}
return found;
}
'

body_random_walk <- '
return wrap(locate());
'
# compile, link and load the C++ function
RandomWalkcpp <- cxxfunction(
  #signature (x="integer"),
  body = body_random_walk,
  includes = incl_random_walk,
  plugin = "Rcpp"
)

total <- 1000000;
walks <- replicate(total, RandomWalkcpp()) 
correct <- sum(walks)
probability_locating <- correct / total

probability_locating

incl_random_walk_c = '
#include<iostream>
#include<cstdlib>

using namespace std;

// constant for where visited
const int VISITED = 1;

bool locate(int numSteps) {

int visited = VISITED;
// create a board size
int N = 1000;
// have offset to prevent setting negative indexing
int OFFSET = 500;
// probably do not need to track the board but just in case
int board[N][N];

// the location to find - must take into account the offset.
int xLocation = OFFSET - 10;
int yLocation = OFFSET + 30;

int curX = 0 + OFFSET;
int curY = 0 + OFFSET;

// keep track of everwhere that was visited in case we want to draw the grid
board[curX][curY] = visited;

srand(1234);

int dir;
int distance = 0;
int totalSteps = 0;
bool debug = false;
bool found = false;
// while less than max steps walked - 1000, and have not found the place then try again
while (totalSteps < numSteps && !found) {
dir = rand() %4 +1;
if (debug) {
cout << dir;
}
// going east
if (dir == 1) {
board[curX][++curY] = visited;
// going west
} else if(dir == 2) {
board[curX][--curY] = visited;
// going north
} else if(dir == 3) {
board[--curX][curY] = visited;
// going south - make these const.
} else if(dir == 4) {
board[++curX][curY] = visited;
}
// check to see if we have found the location
if (curX == xLocation && curY == yLocation) {
found = true;
}
distance++;
totalSteps++;
}
if (found) {
cout << "found";
}
return distance;
}
'

body_random_walk_c <- '
  int maxSteps = as<int>(x);
return wrap(locate(maxSteps));
'
# compile, link and load the C++ function
RandomWalkcppD <- cxxfunction(
  signature (x="integer"),
  body = body_random_walk_c,
  includes = incl_random_walk_c,
  plugin = "Rcpp"
)

total <- 1000000;
walks <- replicate(total, RandomWalkcppD(3000)) 
distance <- sum(walks)
average_distance <- distance / total

# QUESTION 2

setwd('~/Downloads/r_exam')
source('qsort_code.R')

set.seed(1001) #set seed of random number generator
x <- rnorm(10000) #generate some data
#set some randomly selected values to missing
x[ sample(10000,size = 100) ] <- NA 
quickSort(x) 

incl_sort_rcpp = '
void swap(IntegerVector xx, int num1, int num2) {
  int temp = xx[num1];
  xx[num1] = xx[num2];
  xx[num2] = temp;
}

int partition(IntegerVector xx, int left, int right, int pivot) {
  int leftPointer = left -1;
  int rightPointer = right;
  
  while(true) {
    while(xx[++leftPointer] < pivot) {
      //do nothing
    }
    
    while(rightPointer > 0 && xx[--rightPointer] > pivot) {
      //do nothing
    }
    
    if(leftPointer >= rightPointer) {
      break;
    } else {
      swap(xx,leftPointer,rightPointer);
    }
  }
  
  swap(xx,leftPointer,right);
  return leftPointer;
}
void quickSort(IntegerVector xx, int left, int right) {
  if (right-left > 0) {
    int pivot = xx[right];
    int partitionPoint = partition(xx,left, right, pivot);
    quickSort(xx,left,partitionPoint-1);
    quickSort(xx,partitionPoint+1,right);
  }
}
'

# the function body is defined in this R character string
body_SortRcpp <- '
//convert to IntegerVector object - use the clone()
IntegerVector xx = clone(x);
quickSort(xx, 0, xx.size());
return wrap(xx);
'
# compile, link and load the C++ function
SortRcpp <- cxxfunction(
  signature (x="integer"),
  body = body_SortRcpp,
  includes = incl_sort_rcpp,
  plugin = "Rcpp"
)

# call the compiled C++ function from R
SortRcpp(10:1)

incl_sort_stl = '
#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

bool wayToSort(int i, int j) {
  if (i != NA_INTEGER) {
     return i > j;
  }
  return j;
'
body_SortSTL <- '
//convert to IntegerVector object - use the clone()
IntegerVector xx = clone(x);
sort(begin(xx), end(xx));
return wrap(xx);
'

SortSTL <- cxxfunction(
  signature (x="integer"),
  body = body_SortSTL,
  includes = 'using namespace std;', #incl_sort_stl,
  plugin = "Rcpp"
)

SortSTL(10:1)

# handle missing values

# set seed of randon number generator
set.seed(1001)
# generate some data
x < rnorm(10000)
# set some randomly selected values to missing
x[sample(10000, size=100)] <- NA

SortSTL(x)
SortRcpp(x)

benchmark(quickSort(x), SortRcpp(x), SortSTL(x), order = "relative")

library(RcppArmadillo)

#sort a vector
centerRcpp <- cxxfunction( signature(X="numeric"),
                           body = '
                           mat v = as<mat>(X);
                           sort(v);
                           return wrap(c);
                           ', 
                           include = 'using namespace arma;',
                           plugin = "RcppArmadillo")

