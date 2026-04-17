
#include <Rcpp.h>
#include <unordered_map>
#include <cctype>
using namespace Rcpp;

// [[Rcpp::export]]
std::string as_CDateFmt_cpp(std::string fmt) {
  
  static const std::unordered_map<std::string, std::string> map = {
    {"dddd", "%A"},
    {"ddd",  "%a"},
    {"dd",   "%d"},
    {"d",    "%e"},
    {"mmmm", "%B"},
    {"mmm",  "%b"},
    {"mm",   "%m"},
    {"m",    "%m"},
    {"yyyy", "%Y"},
    {"yy",   "%y"},
    {"y",    "%y"}
  };
  
  std::string out;
  out.reserve(fmt.size());
  
  size_t n = fmt.size();
  
  for (size_t i = 0; i < n;) {
    
    char ch = fmt[i];
    
    if (ch == 'd' || ch == 'm' || ch == 'y') {
      
      size_t j = i;
      
      while (j < n && (fmt[j] == 'd' || fmt[j] == 'm' || fmt[j] == 'y')) {
        j++;
      }
      
      std::string block = fmt.substr(i, j - i);
      
      bool left_alpha  = (i > 0 && std::isalpha(fmt[i - 1]));
      bool right_alpha = (j < n && std::isalpha(fmt[j]));
      
      if (left_alpha || right_alpha) {
        out += block;
      } else {
        
        for (size_t k = 0; k < block.size();) {
          
          char c = block[k];
          size_t l = k;
          
          while (l < block.size() && block[l] == c) {
            l++;
          }
          
          std::string token = block.substr(k, l - k);
          
          auto it = map.find(token);
          if (it != map.end()) {
            out += it->second;
          } else {
            out += token;
          }
          
          k = l;
        }
      }
      
      i = j;
      continue;
    }
    
    out += ch;
    i++;
  }
  
  return out;
}

