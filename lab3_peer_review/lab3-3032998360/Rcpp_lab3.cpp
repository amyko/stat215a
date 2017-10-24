

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
// We define a function that, given the vectors of elements 
// in the upper triangular part of two matrices (x and y) and 
// the number of observations that match in two subsamples (q), 
// computes the matching coefficient. 

double similarityC(NumericVector x, NumericVector y, int q){
  int p = x.size(); // Number of elements in vector x. 
  double match = 0; // match is the number of matching elements. 
  double prop = 0;
  
  // For every element in x, if it matches with the element in y, 
  // count 1. 
  for(int i = 0; i < p; i++){
    if (x[i] == y[i]){
      match = match + 1;
    }
  }
  
  // compute the matching coefficient, 
  // which is the proportion of elements in the two matrices that match
  prop = (2*match + q)/(q*q);
  return prop; 
}

