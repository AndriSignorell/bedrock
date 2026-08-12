#include <Rcpp.h>

// std::gcd and std::lcm live in <numeric>, std::reverse in <algorithm>.
// Rcpp.h happens to pull all of them in on the current toolchains, which is
// why the file compiled without them - but that is a property of Rcpp's
// includes, not a guarantee, and it breaks silently on the next toolchain
// that tidies its headers.
#include <numeric>
#include <algorithm>
#include <vector>

using namespace Rcpp;


// --------------------------------
// least common multiple

// [[Rcpp::export]]
long long lcm_cpp(long long int a, long long int b){
  return
  std::lcm(a,b);
}

// --------------------------------
// greatest common divisor

// [[Rcpp::export]]
long long gcd_cpp(long long int a, long long int b){
  return
  std::gcd(a,b);
}


// --------------------------------
// Divisors
//
// Proper divisors of x, in ascending order - every positive divisor except
// x itself.
//
// Trial division up to sqrt(x), where each divisor d found there yields its
// partner x / d for free. The previous version built seq_len(x / 2) and
// tested every one of them: for x = .Machine$integer.max that is 1.07e9
// integers, roughly 4 GB of allocation before the first division, and the
// upper bound in the R wrapper permits exactly that input. It also grew the
// result with IntegerVector::push_back, which reallocates on every call.
// Here the loop runs at most 46341 times for any int, and std::vector grows
// geometrically.
//
// The loop condition is d <= x / d rather than d * d <= x on purpose: at the
// integer limit d reaches 46341, and 46341 * 46341 is 2147488281, past
// INT_MAX. Signed overflow is undefined behaviour, so the multiplication
// would be a bug that only shows up at the largest permitted input - the one
// case nobody tests by hand. The division cannot overflow.

// [[Rcpp::export]]
IntegerVector divs_cpp(int x) {

  // reachable through DescToolsX:::divs_cpp() even though divisors() guards
  // its input, and cheap enough to keep
  if (x < 1)
    stop("'x' must be a positive whole number.");

  // 1 has no PROPER divisor: its only divisor is 1, which is x itself. The
  // loop below would return it, hence the separate exit.
  if (x == 1)
    return IntegerVector(0);

  std::vector<int> lower;
  std::vector<int> upper;

  // a hint, not a bound - the most divisor-rich int, 1745944200, has 1535
  // proper divisors and the vector simply grows
  lower.reserve(32);
  upper.reserve(32);

  for (int d = 1; d <= x / d; ++d) {

    if (x % d != 0)
      continue;

    const int q = x / d;

    // d is always a proper divisor here, because x > 1 and d <= sqrt(x)
    lower.push_back(d);

    // q != x excludes x itself (the partner of d = 1); q != d avoids
    // listing the square root twice when x is a perfect square
    if (q != x && q != d)
      upper.push_back(q);
  }

  // the partners were collected in descending order
  std::reverse(upper.begin(), upper.end());
  lower.insert(lower.end(), upper.begin(), upper.end());

  return wrap(lower);
}
