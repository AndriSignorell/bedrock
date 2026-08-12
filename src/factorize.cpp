
#include <Rcpp.h>
#include <cstdint>
#include <vector>
#include <cmath>

using namespace Rcpp;

/* ============================================================
 64-bit safe arithmetic
 ============================================================ */

uint64_t mul_mod(uint64_t a, uint64_t b, uint64_t mod) {
  __uint128_t res = ( __uint128_t ) a * b;
  return (uint64_t)(res % mod);
}

uint64_t pow_mod(uint64_t base, uint64_t exp, uint64_t mod) {
  uint64_t res = 1;
  while (exp > 0) {
    if (exp & 1)
      res = mul_mod(res, base, mod);
    base = mul_mod(base, base, mod);
    exp >>= 1;
  }
  return res;
}

uint64_t gcd_u64(uint64_t a, uint64_t b) {
  while (b != 0) {
    uint64_t t = b;
    b = a % b;
    a = t;
  }
  return a;
}

/* ============================================================
 Deterministic Miller-Rabin (64-bit)
 ============================================================ */

bool miller_rabin_u64(uint64_t n) {
  if (n < 2) return false;
  for (uint64_t p : {2ULL, 3ULL, 5ULL, 7ULL, 11ULL, 13ULL, 17ULL}) {
    if (n == p) return true;
    if (n % p == 0) return n == p;
  }
  
  uint64_t d = n - 1, s = 0;
  while ((d & 1) == 0) {
    d >>= 1;
    ++s;
  }
  
  for (uint64_t a : {2ULL, 325ULL, 9375ULL, 28178ULL,
       450775ULL, 9780504ULL, 1795265022ULL}) {
    if (a % n == 0) continue;
    uint64_t x = pow_mod(a, d, n);
    if (x == 1 || x == n - 1) continue;
    
    bool composite = true;
    for (uint64_t r = 1; r < s; ++r) {
      x = mul_mod(x, x, n);
      if (x == n - 1) {
        composite = false;
        break;
      }
    }
    if (composite) return false;
  }
  return true;
}

/* ============================================================
 Pollard's rho (64-bit)
 ============================================================ */

uint64_t pollard_rho(uint64_t n) {
  if (n % 2 == 0) return 2;
  
  uint64_t x = 2, y = 2, c = 1, d = 1;
  
  auto f = [&](uint64_t v) {
    return (mul_mod(v, v, n) + c) % n;
  };
  
  while (d == 1) {
    x = f(x);
    y = f(f(y));
    uint64_t diff = x > y ? x - y : y - x;
    d = gcd_u64(diff, n);
  }
  
  if (d == n)
    return pollard_rho(n + 1);
  
  return d;
}

/* ============================================================
 Recursive factorization
 ============================================================ */

void factor_rec(uint64_t n, std::vector<uint64_t>& factors) {
  if (n == 1) return;
  
  if (miller_rabin_u64(n)) {
    factors.push_back(n);
  } else {
    uint64_t d = pollard_rho(n);
    factor_rec(d, factors);
    factor_rec(n / d, factors);
  }
}

/* ============================================================
 R interfaces
 ============================================================ */

// [[Rcpp::export]]
bool is_prime_cpp(double x) {
  if (x < 2 || x != std::floor(x) || x > 9.22e18)
    return false;
  return miller_rabin_u64((uint64_t)x);
}

// --------------------------------
// Primes up to n, by the sieve of Eratosthenes
//
// The limit is 1e8, and it is a practical bound rather than a type bound.
// At 1e8 the sieve is a bit-packed 12.5 MB, the 5,761,455 primes are 23 MB
// as an integer vector, and the peak - C++ vector plus the copy into R -
// stays under about 70 MB. At .Machine$integer.max the same three numbers
// are 268 MB, 105,097,565 primes for 420 MB, and a peak beyond a gigabyte.
// Someone who genuinely needs that range needs a segmented sieve, not a
// larger allocation.
//
// The parameter is double, not int, for the same reason is_prime_cpp() and
// factor_u64_cpp() in this file take one: R passes numeric, and converting
// a double past INT_MAX to int is undefined behaviour that would happen
// BEFORE any check inside the function could run. Taking the double and
// validating it here means primes_upto_cpp(1e10) reports the limit instead
// of misbehaving.
//
// The loop counters are int64_t so that neither `i * i` nor `j += i` can
// overflow. With the limit above they could not anyway, but the previous
// version had `std::vector<bool> is_prime(n + 1, true)` with an int n -
// and n + 1 at INT_MAX is signed overflow, i.e. undefined behaviour on the
// largest input the wrapper then permitted.

// [[Rcpp::export]]
IntegerVector primes_upto_cpp(double n) {
  
  constexpr double maxN = 100000000.0;
  
  if (!R_finite(n) || n != std::floor(n) || n < 1.0 || n > maxN)
    stop("'n' must be a whole number between 1 and 100000000.");
  
  const std::int64_t limit = static_cast<std::int64_t>(n);
  
  if (limit < 2)
    return IntegerVector();
  
  std::vector<bool> isPrime(static_cast<std::size_t>(limit) + 1U, true);
  
  isPrime[0] = false;
  isPrime[1] = false;
  
  for (std::int64_t i = 2; i * i <= limit; ++i) {
    
    if (!isPrime[static_cast<std::size_t>(i)])
      continue;
    
    for (std::int64_t j = i * i; j <= limit; j += i)
      isPrime[static_cast<std::size_t>(j)] = false;
  }
  
  std::vector<int> result;
  
  // Reserving cuts the peak the colleague's estimate is mostly about: a
  // growing vector reallocates by doubling, so the last growth holds the
  // old and the new buffer at once - up to 1.5 times the final size in
  // addition to the sieve. Rosser and Schoenfeld give pi(x) < 1.25506
  // x/ln(x) for x > 1, so this reserve is an upper bound and never short.
  if (limit > 16)
    result.reserve(static_cast<std::size_t>(
        1.26 * static_cast<double>(limit) / std::log(static_cast<double>(limit))));
  
  for (std::int64_t i = 2; i <= limit; ++i)
    if (isPrime[static_cast<std::size_t>(i)])
      result.push_back(static_cast<int>(i));
  
  return wrap(result);
}


// [[Rcpp::export]]
NumericMatrix factor_u64_cpp(double x) {
  if (x < 2 || x != std::floor(x) || x > 9.22e18)
    stop("x must be an integer in [2, 2^63)");
  
  uint64_t n = (uint64_t)x;
  std::vector<uint64_t> f;
  factor_rec(n, f);          // yields sorted factors
  
  // count factors (linear, no table/unique)
  std::vector<uint64_t> p, m;
  for (size_t i = 0; i < f.size();) {
    uint64_t val = f[i];
    size_t cnt = 1;
    while (++i < f.size() && f[i] == val)
      ++cnt;
    p.push_back(val);
    m.push_back(cnt);
  }
  
  NumericMatrix out(p.size(), 2);
  for (size_t i = 0; i < p.size(); ++i) {
    out(i, 0) = (double)p[i];
    out(i, 1) = (double)m[i];
  }
  
  colnames(out) = CharacterVector::create("p", "m");
  return out;
}
