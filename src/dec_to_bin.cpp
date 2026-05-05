
#include <Rcpp.h>
#include <bitset>
using namespace Rcpp;


// [[Rcpp::export]]
CharacterVector dec_to_bin_cpp(IntegerVector n)
{
  const unsigned g_unMaxBits = 32;
  int len = n.size();
  
  CharacterVector out(len);
  
  for(int i = 0; i < len; i++) {
    std::bitset<g_unMaxBits> b(n[i]);
    out[i] = b.to_string();
  }
  
  return out;
}
