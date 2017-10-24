#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double JaccardCPP(Rcpp::NumericVector data_1, Rcpp::NumericVector data_2){
  int n = data_1.size(); // initialize an empty vector of size equal to the first subsample
  
  int NV11 = 0;  // initialize vectors for similarity measures
  int NV00 = 0;
  int NV10 = 0;
  int NV01 = 0;
  // start i at 0, increment by 1 (++) as long as 
  // i is less than the size of the subsample
  for (int i = 0 ; i < n ; i++){ 
    for (int j = i+1 ; j < n ; j++){ // do the same for j
      // create an index for when i = j for first subsample
      int index1 = (data_1[i] == data_1[j]);
      // same as above for second subsample
      int index2 = (data_2[i] == data_2[j]); 
      // create a series of logical conditions for if the index is similar; 
      // If the condition matches, then increment (++), if not, move to the next condition
      if ( index1 &&  index2){
        NV11++; 
      }
      if (!index1 && !index2){
        NV00++;
      }
      if (!index1 &&  index2){
        NV01++;
      }
      if ( index1 && !index2){
        NV10++;
      }
    }
  }
  // take the number of times the observations were the same, divide by the total observations
  return float(NV11)/float(NV10 + NV01 + NV11); 
}