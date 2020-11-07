#include <iostream>
#include <Rcpp.h>
#include <limits>
#include <vector>
#include <forward_list>
#include <functional>
#include <cmath>

// Enable C++11 via plugin
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using namespace std::placeholders;

// roll FUN on window starting at current-left_bound and ending at
// current_date-right_bound.
NumericVector c_roll(std::function<double (int si, int ei, const NumericVector& X)> fun,
                     NumericVector& DATE,
                     NumericVector& X,
                     double left_bound,
                     double right_bound,
                     bool left_open,
                     bool right_open,
                     double fill){
  if(DATE.length() != X.length()){
	Rf_error("Length of 'IX' and 'X' vectors differ.");
  }
  if(right_bound < left_bound){
    Rf_error("`left_bound` is larger than the `right_bound`");
  }

  int N = DATE.length();
  int Nm1 = N - 1;
  std::forward_list<double> out_vals, out_dates;

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

    /* std::cout << "ri:" << ri << " rd:" << rd << " sd:" << sd << " ed:" << ed << " si:" << */
    /*   si << " ei:" << ei - 1  << " X[ei]:" << X[ei-1] << std::endl; */

    if (is_outside) {
      val = fill;
    } else {
      val = fun(si, ei - 1, X);
    }

    while (nsame > 0){
      nsame--;
      out_vals.push_front(val);
    }
  }

  // out_dates.reverse();
  out_vals.reverse();

  // NumericVector ix = NumericVector(out_dates.begin(), out_dates.end());
  // NumericVector vals = NumericVector(out_vals.begin(), out_vals.end());

  // return DataFrame::create(Named("ix") = out_dates,
  //   					   Named("val") = out_vals);
  return NumericVector(out_vals.begin(), out_vals.end());
}

// receive start index (si), end index + 1 (ei) an the original vector X
inline double vec_min(int si, int ei, const NumericVector& X) {
  double accum = R_PosInf;
  for(int i = si; i <= ei; i++) {
    if (!ISNA(X[i]))
      accum = std::min(accum, X[i]);
  }
  if (accum == R_PosInf) return NA_REAL;
  else return accum;
}

inline double vec_max(int si, int ei, const NumericVector& X) {
  double accum = R_NegInf;
  for(int i = si; i <= ei; i++){
    if (!ISNA(X[i]))
      accum = std::max(accum, X[i]);
  }
  if (accum == R_NegInf) return NA_REAL;
  else return accum;
}

double vec_mean(int si, int ei, const NumericVector& X) {
  double accum = 0;
  size_t nr = 0;
  for(int i = si; i <= ei; i++){
    // this NA check doesn't work on integers
    if (!ISNA(X[i])){
      accum += X[i];
      nr++;
    }
  }
  if (nr) return accum/nr;
  else return NA_REAL;
}

double vec_sd(int si, int ei, const NumericVector& X) {
  double mean = vec_mean(si, ei, X);
  double accum = 0.0;
  size_t nr = 0;
  for(int i = si; i <= ei; i++){
    // this NA check doesn't work on integers
    if (!ISNA(X[i])){
      accum += std::pow(X[i] - mean, 2.0);
      nr++;
    }
  }
  if (nr) return std::sqrt(accum/nr);
  else return NA_REAL;
}

double vec_sum(int si, int ei, const NumericVector& X) {
  double accum = 0.0;
  for(int i = si; i <= ei; i++){
    if (!ISNA(X[i])) {
      accum += X[i];
    }
  }
  return accum;
}

double vec_prod(int si, int ei, const NumericVector& X) {
  double accum = 1.0;
  for(int i = si; i <= ei; i++){
    if (!ISNA(X[i])) {
      accum *= X[i];
    }
  }
  return accum;
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_min(SEXP DATE, NumericVector& X,
                         double left_bound, double right_bound,
                         bool left_open, bool right_open,
                         double fill){
  NumericVector tt(DATE);
  return c_roll(vec_min, tt, X, left_bound, right_bound, left_open, right_open, fill);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_max(SEXP DATE, NumericVector& X,
                         double left_bound, double right_bound,
                         bool left_open, bool right_open,
                         double fill){
  NumericVector tt(DATE);
  return c_roll(vec_max, tt, X, left_bound, right_bound, left_open, right_open, fill);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_mean(SEXP DATE, NumericVector& X,
                          double left_bound, double right_bound,
                          bool left_open, bool right_open,
                          double fill){
  NumericVector tt(DATE);
  return c_roll(vec_mean, tt, X, left_bound, right_bound, left_open, right_open, fill);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_sd(SEXP DATE, NumericVector& X,
                        double left_bound, double right_bound,
                        bool left_open, bool right_open,
                        double fill){
  NumericVector tt(DATE);
  return c_roll(vec_sd, tt, X, left_bound, right_bound, left_open, right_open, fill);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_sum(SEXP DATE, NumericVector& X,
                         double left_bound, double right_bound,
                         bool left_open, bool right_open,
                         double fill){
  NumericVector tt(DATE);
  return c_roll(vec_sum, tt, X, left_bound, right_bound, left_open, right_open, fill);
}


//' @export
// [[Rcpp::export]]
NumericVector c_roll_prod(SEXP DATE, NumericVector& X,
                          double left_bound, double right_bound,
                          bool left_open, bool right_open,
                          double fill){
  NumericVector tt(DATE);
  return c_roll(vec_prod, tt, X, left_bound, right_bound, left_open, right_open, fill);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_first(SEXP DATE, NumericVector& X,
                           double left_bound, double right_bound,
                           bool left_open, bool right_open,
                           double fill){
  NumericVector tt(DATE);
  return c_roll([](int si,  int ei, const NumericVector& X) { return X[si]; },
				tt, X, left_bound, right_bound, left_open, right_open, fill);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_last(SEXP DATE, NumericVector& X,
                          double left_bound, double right_bound,
                          bool left_open, bool right_open,
                          double fill){
  NumericVector tt(DATE);
  return c_roll([](int si, int ei, const NumericVector& X) { return X[ei]; },
				tt, X, left_bound, right_bound, left_open, right_open, fill);
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

double vec_quantile(int si,  int ei, const NumericVector& X, double prob){
  // quantile setup
  printf("se: %d %d\n", si, ei);
  NumericVector::const_iterator first = X.begin() + si;
  NumericVector::const_iterator last = X.begin() + (ei + 1);
  std::vector<double> z(first, last);
  printf("len: %d\n", (int) z.size());
  printf("sevals: %f %f\n", z.front(), z.back());
  sort(z.begin(), z.end());
  return quantile(z, prob);
}

//' @export
// [[Rcpp::export]]
NumericVector c_roll_quantile(SEXP DATE, NumericVector& X,
                              double left_bound, double right_bound,
                              bool left_open, bool right_open,
                              double prob,
                              double fill){
  NumericVector tt(DATE);
  return c_roll(std::bind(vec_quantile, _1, _2, _3, prob),
				tt, X, left_bound, right_bound, left_open, right_open, fill);
}


// UTILS

// like base cummin but ignores NAs
//' @export
// [[Rcpp::export]]
NumericVector c_cummin(const NumericVector& X){
  NumericVector out(X.length());
  if (X.length()){
    double accum = X[0];
    out[0] = accum;
    for (int i = 1; i < X.length(); i++){
      if (!ISNA(X[i]) && X[i] != NA_INTEGER){
        accum = std::min(X[i], accum);
      }
      out[i] = accum;
    }
  }
  return out;
}


// like base cummax but ignores NAs
//' @export
// [[Rcpp::export]]
NumericVector c_cummax(const NumericVector& X){
  NumericVector out(X.length());
  if (X.length()){
    double accum = X[0];
    out[0] = accum;
    for (int i = 1; i < X.length(); i++){
      if (!ISNA(X[i]) && X[i] != NA_INTEGER){
        accum = std::max(X[i], accum);
      }
      out[i] = accum;
    }
  }
  return out;
}
