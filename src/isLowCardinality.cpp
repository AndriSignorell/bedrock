
#include <Rcpp.h>
#include <unordered_set>
using namespace Rcpp;

// [[Rcpp::export]]
bool isLowCardinality(SEXP x, int maxUnique = 12) {
  
  if (TYPEOF(x) == REALSXP) {
    
    NumericVector xv(x);
    std::unordered_set<double> seen;
    seen.reserve(std::min((R_xlen_t)xv.size(), (R_xlen_t)(maxUnique + 1)));
    
    for (double v : xv) {
      if (NumericVector::is_na(v)) continue;
      seen.insert(v);
      if ((int)seen.size() > maxUnique) return false;
    }
    return true;
    
  } else if (TYPEOF(x) == INTSXP) {
    
    IntegerVector xv(x);
    std::unordered_set<int> seen;
    seen.reserve(std::min((R_xlen_t)xv.size(), (R_xlen_t)(maxUnique + 1)));
    
    for (int v : xv) {
      if (v == NA_INTEGER) continue;
      seen.insert(v);
      if ((int)seen.size() > maxUnique) return false;
    }
    return true;
    
  } else {
    stop("isLowCardinality: 'x' must be numeric or integer.");
  }
}

