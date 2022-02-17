#include <cmath>
#include <cpp11.hpp>
#include <forward_list>
#include <functional>
#include <iostream>
#include <limits>
#include <vector>
#include <algorithm>

using namespace std::placeholders;

// roll FUN on window starting at current-left_bound and ending at
// current_date-right_bound.
cpp11::writable::doubles
c_roll(std::function<double (int si, int ei, const cpp11::doubles& X)> fun,
       cpp11::doubles DATE,
       cpp11::doubles X,
       double left_bound,
       double right_bound,
       bool left_open,
       bool right_open,
       double fill){
  if(DATE.size() != X.size()){
	Rf_error("Length of 'IX' and 'X' vectors differ.");
  }
  if(right_bound < left_bound){
    Rf_error("`left_bound` is larger than the `right_bound`");
  }

  int N = DATE.size();
  int Nm1 = N - 1;
  std::forward_list<double> out_vals;

  // ref, start and end indexes
  int ri = 0; // ref index
  int si = 0; // start index, ix of the first element in the window queue
  int ei = 1; // end index, ix of the last element in the window

  double val;

  while(ri < N) {

	double rd = DATE[ri];         // ref date
	double sd = rd + left_bound;  // start date (of the window)
	double ed = rd + right_bound; // end date (of the window)

	// move ri to next date;
	ri++;
    int nsame = 1;
	while(ri < N && rd == DATE[ri]){
	  ri++;
      nsame++;
	}

    // Make interval sd <= date <= ed
    if (left_open)
      while(si < Nm1 & DATE[si] <= sd) si++;
    else
      while(si < Nm1 & DATE[si] < sd) si++;

    if (right_open)
      while(ei < N & DATE[ei] < ed) ei++;
    else
      while(ei < N & DATE[ei] <= ed) ei++;


    bool is_outside =
      ei <= si ||
      (right_open ? DATE[si] >= ed : DATE[si] > ed) ||
      (left_open ? DATE[ei-1] <= sd : DATE[ei-1] < sd);

    if (is_outside) {
      val = fill;
    } else {
      val = fun(si, ei, X);
    }

    /* std::cout << "ri:" << ri << " rd:" << rd << " sd:" << sd << " ed:" << ed */
    /*           << " si:" << si << " ei:" << ei - 1 << " X[ei]:" << X[ei - 1] */
    /*           << " val:" << val */
    /*           << std::endl; */

    while (nsame > 0){
      nsame--;
      out_vals.push_front(val);
    }
  }

  /* Rf_PrintValue(X); */

  cpp11::writable::doubles out(N);
  for (size_t i = N; i > 0;) {
    out[--i] = out_vals.front();
    out_vals.pop_front();
  }

  return out;
}

// receive start index (si), end index + 1 (ei) an the original vector X
inline double vec_min(int si, int ei, const cpp11::doubles X) {
  double accum = R_PosInf;
  for(int i = si; i < ei; i++) {
    double x = X[i];
    if (!ISNA(x))
      accum = std::min(accum, x);
  }
  if (accum == R_PosInf) return NA_REAL;
  else return accum;
}

inline double vec_max(int si, int ei, const cpp11::doubles X) {
  double accum = R_NegInf;
  std::cout << "accum:" << accum << std::endl;
  Rf_PrintValue(X);
  for (size_t i = si; i < ei; i++) {
    double x = X[i];
    std::cout << "i:" << i << " X[i]:" << x << std::endl;
    if (!ISNA(X[i]))
      accum = std::max(accum, x);
  }
  std::cout << "accum:" << accum << std::endl;
  if (accum == R_NegInf) return NA_REAL;
  else return accum;
}

double vec_mean(int si, int ei, const cpp11::doubles X) {
  double accum = 0;
  size_t nr = 0;
  for(int i = si; i < ei; i++){
    // this NA check doesn't work on integers
    if (!ISNA(X[i])){
      accum += X[i];
      nr++;
    }
  }
  if (nr) return accum/nr;
  else return NA_REAL;
}

double vec_sd(int si, int ei, const cpp11::doubles X) {
  double mean = vec_mean(si, ei, X);
  double accum = 0.0;
  size_t nr = 0;
  for(int i = si; i < ei; i++){
    // this NA check doesn't work on integers
    if (!ISNA(X[i])){
      accum += std::pow(X[i] - mean, 2.0);
      nr++;
    }
  }
  if (nr) return std::sqrt(accum/nr);
  else return NA_REAL;
}

double vec_sum(int si, int ei, const cpp11::doubles X) {
  double accum = 0.0;
  for(int i = si; i < ei; i++){
    if (!ISNA(X[i])) {
      accum += X[i];
    }
  }
  return accum;
}

double vec_prod(int si, int ei, const cpp11::doubles X) {
  double accum = 1.0;
  for(int i = si; i < ei; i++){
    if (!ISNA(X[i])) {
      accum *= X[i];
    }
  }
  return accum;
}

[[cpp11::register]]
cpp11::doubles c_roll_min(cpp11::doubles ix, const cpp11::doubles& X, double left_bound,
               double right_bound, bool left_open, bool right_open,
               double fill) {
  return c_roll(vec_min, ix, X, left_bound, right_bound, left_open, right_open, fill);
}

[[cpp11::register]]
cpp11::doubles c_roll_max(cpp11::doubles ix, const cpp11::doubles& X,
                          double left_bound,
                          double right_bound, bool left_open,
                          bool right_open, double fill) {
  return c_roll(vec_max, ix, X, left_bound, right_bound, left_open, right_open, fill);
}

[[cpp11::register]]
cpp11::doubles c_roll_mean(
  cpp11::doubles ix, const cpp11::doubles& X, double left_bound, double right_bound,
    bool left_open, bool right_open, double fill) {
  return c_roll(vec_mean, ix, X, left_bound, right_bound, left_open, right_open, fill);
}

[[cpp11::register]]
cpp11::doubles c_roll_sd(cpp11::doubles ix, const cpp11::doubles& X, double left_bound, double right_bound,
          bool left_open, bool right_open, double fill) {
  return c_roll(vec_sd, ix, X, left_bound, right_bound, left_open, right_open, fill);
}

[[cpp11::register]]
cpp11::doubles c_roll_sum(cpp11::doubles ix, const cpp11::doubles& X, double left_bound,
                          double right_bound, bool left_open, bool right_open,
                          double fill) {
  return c_roll(vec_sum, ix, X, left_bound, right_bound, left_open, right_open, fill);
}

[[cpp11::register]]
cpp11::doubles c_roll_prod(cpp11::doubles ix, const cpp11::doubles& X,
                           double left_bound, double right_bound,
                           bool left_open, bool right_open,
                           double fill){
  return c_roll(vec_prod, ix, X, left_bound, right_bound, left_open, right_open, fill);
}

[[cpp11::register]]
cpp11::doubles c_roll_first(cpp11::doubles ix, const cpp11::doubles& X,
                            double left_bound, double right_bound,
                            bool left_open, bool right_open,
                            double fill){
  return c_roll([](int si, int ei, const cpp11::doubles X) { return X[si]; },
				ix, X, left_bound, right_bound, left_open, right_open, fill);
}

[[cpp11::register]]
cpp11::doubles c_roll_last(cpp11::doubles ix, const cpp11::doubles& X,
                           double left_bound, double right_bound,
                           bool left_open, bool right_open,
                           double fill){
  return c_roll([](int si, int ei, const cpp11::doubles& X) { return X[ei - 1]; },
				ix, X, left_bound, right_bound, left_open, right_open, fill);
}


// QUANTILES
double quantile(const std::vector<double>& z, double prob) {
  int n = z.size();
  if(n == 1){
	return z[0];
  } else {
	int lo = floor((n - 1) * prob);
	int hi = lo + 1;
	return z[lo]*prob + z[hi]*(1 - prob);
  }
}

double vec_quantile(int si, int ei, const cpp11::doubles& X, double prob){
  /* printf("se: %d %d\n", si, ei); */
  const auto& first = X.begin() + si;
  const auto& last = X.begin() + ei;
  std::vector<double> z(first, last);
  /* printf("len: %d\n", (int) z.size()); */
  /* printf("sevals: %f %f\n", z.front(), z.back()); */
  sort(z.begin(), z.end());
  return quantile(z, prob);
}

[[cpp11::register]]
cpp11::doubles c_roll_quantile(cpp11::doubles ix, const cpp11::doubles& X,
                               double left_bound, double right_bound,
                               bool left_open, bool right_open,
                               double prob,
                               double fill) {
  return c_roll(std::bind(vec_quantile, _1, _2, _3, prob),
				ix, X, left_bound, right_bound, left_open, right_open, fill);
}

// UTILS
// like base cummin but ignores NAs
[[cpp11::register]]
cpp11::doubles c_cummin(const cpp11::doubles X) {
  cpp11::writable::doubles out(X.size());
  size_t N = X.size();
  if (N > 0){
    double accum = X[0];
    out[0] = accum;
    for (int i = 1; i < N; i++){
      double x = X[i];
      if (!ISNA(x) && x != NA_INTEGER) {
        accum = std::min(x, accum);
      }
      out[i] = accum;
    }
  }
  return out;
}

// like base cummax but ignores NAs
[[cpp11::register]]
cpp11::doubles c_cummax(const cpp11::doubles X){
  cpp11::writable::doubles out(X.size());
  size_t N = X.size();
  if (N) {
    double accum = X[0];
    out[0] = accum;
    for (int i = 1; i < N; i++){
      double x = X[i];
      if (!ISNA(x) && x != NA_INTEGER) {
        accum = std::max(x, accum);
      }
      out[i] = accum;
    }
  }
  return out;
}
