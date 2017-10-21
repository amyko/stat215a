#include <Rcpp.h>

// The line [[Rcpp::export]] before a function tells R to treat it like
// a native function.
// [[Rcpp::export]]
Rcpp::NumericVector correlationCPP(Rcpp::NumericVector x, Rcpp::NumericVector y) {
// Calculate the correlation measure between x and y
// Input: x, numeric vector of size n; cluster assignment for sample 1
//        y, numeric vector of size n; cluster assignment for sample 2
// NOTE: For each row i, x[i] and y[i] must correspond to the same individual
// Output: corr, numeric vector of size 1; correlation between x and y

  // initialize output values
  //l1_l2 is <L1, L2> described in Ben-Hur, and so on
  double l1_l2 = 0.0;
  double l1_l1 = 0.0;
  double l2_l2 = 0.0;
  
  // This is the length of the x vector.
  int n = x.size();
  
  // Check that the size is the same and return NA if it is not.
  if (y.size() != n) {
    Rcpp::Rcout << "Error: the size of x and y must be the same.\n";
    return(Rcpp::NumericVector::create(NA_REAL));
  }

  // Compute <L1, L1>, <L2,L2>, <L1, L2> described in Ben-Hur
  // Compute the above dot products by computing each C_ij
  // Iterate through every pair (i,j)
  for (int i = 0; i < n; i++) {
    
    for(int j = i+1; j<n; j++){
     
     //compute C_ij for x and for y
     // C_ij = 1 if i and j are in the same cluster; 0 otherwise
      double c1_ij = (x[i] == x[j]) ? 1 : 0;
      double c2_ij = (y[i] == y[j]) ? 1 : 0;
      
      //update the dot product
      l1_l2 += c1_ij * c2_ij;
      l1_l1 += c1_ij * c1_ij;
      l2_l2 += c2_ij * c2_ij;
      
    }
    
  }

  //compute correlation
  double corr = l1_l2 / (sqrt(l1_l1) * sqrt(l2_l2));

  // We need to convert between the double type and the R numeric vector type.
  // Return correlation.
  return Rcpp::NumericVector::create(corr);

}

