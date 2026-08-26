#include <Rcpp.h>
#include <cstdint>
#include <vector>
#include <cmath>
#include <algorithm>
#include <initializer_list>

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
 
 Returns a NON-TRIVIAL divisor of the composite n.
 
 The cycle-finding walk x -> x^2 + c mod n can fail: Floyd's tortoise and
 hare may meet before the gcd separates a factor, in which case the gcd is
 n itself. The only correct reaction is to walk the SAME n again with a
 different c, because a different c is a different pseudo-random sequence.
 
 The previous version did `return pollard_rho(n + 1)` here, i.e. it went
 looking for a divisor of a DIFFERENT NUMBER and handed it back as if it
 divided n. Two things then went wrong at once: the divisor is generally
 not a divisor of n, and `n / d` in factor_rec truncates, so the recursion
 continued on a number unrelated to the input. factorize(21) returned
 2, 2, 5 and factorize(25) returned 2, 2, 2, 3. Over 2..20000 the product
 of the reported factors differed from the input for 2582 of the 19999
 numbers, so this was not an edge case.
 
 Termination: for odd composite n the loop over c ends as soon as one c
 splits n. Empirically c never exceeded 3 (checked over all n in 2..300000
 and 2000 random n below 2^53); there is no proof that some c must work,
 which is why the loop is written over c rather than as a fixed retry.
 ============================================================ */

uint64_t pollard_rho(uint64_t n) {
  if (n % 2 == 0) return 2;
  
  for (uint64_t c = 1; ; ++c) {
    
    uint64_t x = 2, y = 2, d = 1;
    
    // gcd(0, n) == n, so a closed cycle (x == y) leaves the loop with
    // d == n and is handled by the retry below like any other failure.
    while (d == 1) {
      x = (mul_mod(x, x, n) + c) % n;
      y = (mul_mod(y, y, n) + c) % n;
      y = (mul_mod(y, y, n) + c) % n;
      uint64_t diff = x > y ? x - y : y - x;
      d = gcd_u64(diff, n);
    }
    
    if (d != n)
      return d;
  }
}

/* ============================================================
 Recursive factorization
 
 Yields the prime factors of n in ARBITRARY order: rho splits n into two
 parts of unpredictable size, so 35 comes out as 7, 5 and 63 as 3, 7, 3.
 The caller must sort before counting multiplicities.
 ============================================================ */

void factor_rec(uint64_t n, std::vector<uint64_t>& factors) {
  if (n < 2) return;               // 1 has the empty factorization
  
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

// --------------------------------
// Primality of x.
//
// x < 2, non-integer and non-finite values are FALSE by definition of the
// predicate - there the answer is known, it is just not "prime".
//
// Above 2^53 there is no answer to give: a double does not represent every
// integer up there, so the value that arrives is not necessarily the value
// that was typed. R parses 9007199254740997 - a prime - as
// 9007199254740996, and every representable double above 2^53 is even, so
// a test of the neighbour reports FALSE for EVERY prime beyond the bound.
// Returning false would be a confident wrong answer, hence stop().
// isPrime() masks these elements out and reports them as NA with a
// warning - that is the wrapper's own decision, not a precondition of this
// function, so anything calling is_prime_cpp() directly gets the error.
// The two files have to move together: with an isPrime() that does not
// mask, oversized elements arrive here and abort the whole call.

// [[Rcpp::export]]
bool is_prime_cpp(double x) {
  
  constexpr double maxX = 9007199254740992.0;   // 2^53
  
  // non-finite first, so that Inf and NaN follow the predicate rule
  // rather than the range check below
  if (!R_finite(x) || x < 2 || x != std::floor(x))
    return false;
  
  if (x > maxX)
    stop("'x' must not exceed 2^53; above this bound, not every integer "
         "can be represented exactly by a double.");
  
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


// --------------------------------
// Prime factorization of x, as a two-column matrix (p, m).
//
// The upper bound is 2^53 and matches factorize(): it is the largest
// integer up to which EVERY integer is representable. Above it the
// representable integers thin out - 2^53 + 1 is not one, 2^53 + 2 is - so
// the value arriving here need not be the value the user typed, and the
// factorization would be correct for a number nobody asked about. The
// bound is written as the exact constant rather than as 9.22e18, so that
// the check and the message agree.
//
// x == 1 returns a zero-row matrix: the empty product. That keeps the
// invariant prod(p^m) == x valid at the lower end of the documented range
// and is what R's prod(numeric(0)) == 1 does anyway.

// [[Rcpp::export]]
NumericMatrix factor_u64_cpp(double x) {
  
  constexpr double maxX = 9007199254740992.0;   // 2^53
  
  if (!R_finite(x) || x != std::floor(x) || x < 1.0 || x > maxX)
    stop("'x' must be a whole number between 1 and 2^53.");
  
  uint64_t n = (uint64_t)x;
  std::vector<uint64_t> f;
  factor_rec(n, f);
  
  // factor_rec splits, it does not order - see the comment there. Without
  // this sort the counting loop below sees 63 as 3, 7, 3 and reports the
  // two threes as two separate rows with multiplicity one.
  std::sort(f.begin(), f.end());
  
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
  
  // Set explicitly rather than through colnames(), which needs a matrix
  // that already has dimnames or rows to hang them on; x == 1 gives a
  // zero-row matrix.
  out.attr("dimnames") = List::create(R_NilValue,
           CharacterVector::create("p", "m"));
  
  return out;
}
