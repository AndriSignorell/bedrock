
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::IntegerVector binary_tree_cpp(int n) {
  
  if(n < 1)
    Rcpp::stop("n must be >= 1");
  
  Rcpp::IntegerVector ranks(n);
  
  // remaining indices
  std::vector<int> rem(n);
  for(int i = 0; i < n; ++i)
    rem[i] = i;
  
  int depth = std::floor(std::log2(n));
  int start = 1 << depth;
  
  int m = n;
  
  // ---- last row ----
    int lastrow_length = 1 + n - start;
  
  std::vector<int> new_rem;
  new_rem.reserve(n);
  
  for(int i = 0; i < lastrow_length; ++i) {
    int idx = rem[2*i];
    ranks[idx] = start + i;
  }
  
  // build new remaining
  for(int i = 0; i < m; ++i) {
    if(i % 2 == 1 || i/2 >= lastrow_length)
      new_rem.push_back(rem[i]);
  }
  
  rem.swap(new_rem);
  m = rem.size();
  
  // ---- upper levels ----
    while(start > 1) {
      
      start /= 2;
      
      new_rem.clear();
      
      for(int i = 0; i < start; ++i) {
        int idx = rem[2*i];
        ranks[idx] = start + i;
      }
      
      for(int i = 0; i < m; ++i) {
        if(i % 2 == 1 || i/2 >= start)
          new_rem.push_back(rem[i]);
      }
      
      rem.swap(new_rem);
      m = rem.size();
    }
  
  return ranks;
}

