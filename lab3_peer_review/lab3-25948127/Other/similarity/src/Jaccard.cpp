#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double JaccardCPP(Rcpp::NumericVector data1, Rcpp::NumericVector data2){
  int n = data1.size();
  
  int N11 = 0; 
  int N00 = 0;
  int N10 = 0;
  int N01 = 0;
  for (int i = 0 ; i < n ; i++){
    for (int j = i+1 ; j < n ; j++){
      int ind1 = (data1[i] == data1[j]);
      int ind2 = (data2[i] == data2[j]);
      if ( ind1 &&  ind2){
        N11++;
      }
      if (!ind1 && !ind2){
        N00++;
      }
      if (!ind1 &&  ind2){
        N01++;
      }
      if ( ind1 && !ind2){
        N10++;
      }
    }
  }
  return float(N11)/float(N10 + N01 + N11);
}