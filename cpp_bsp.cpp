#include <Rcpp.h>
#include <algorithm>
#include <iterator>

using namespace Rcpp;

// [[Rcpp::export]]
double which_minC(NumericVector x){
  int out;
  out = std::distance(x.begin(),std::min_element(x.begin(),x.end()));
  out++;
  
  return out;
}