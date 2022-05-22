#include "cpp11/integers.hpp"
#include <cpp11.hpp>
#include <vector>
using std::vector;

[[cpp11::register]]
cpp11::doubles c_cumema(cpp11::doubles X, cpp11::doubles days, double n) {
  if (n == 0) {
    return X;
  }
  size_t N = X.size();
  cpp11::writable::doubles out(N);

  if (N > 0) {
    size_t prev = 0;
    while (ISNA(X[prev] && prev < N)) {
      out[prev] = NA_REAL;
      prev++;
    }
    out[prev] = X[prev];

    for(size_t i = prev + 1; i < N; i++){
      if (ISNA(X[i])) {
        out[i] = NA_REAL;
      } else {
        double delta = days[i] - days[prev];
        if( delta < 0) Rf_error("days argument is not increasing");
        double W = exp(-delta/n);
        out[i] = W*out[prev] + X[i];
        prev = i;
      }
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::doubles c_ema(cpp11::doubles X, cpp11::doubles days, double n){
  if (n == 0) {
    return X;
  }

  size_t N = X.size();
  cpp11::writable::doubles out(N);

  if (N > 0) {
    size_t prev = 0;
    while (ISNA(X[prev]) && prev < N) {
      out[prev] = NA_REAL;
      prev++;
    }
    out[prev] = X[prev];
    for (size_t i = prev + 1; i < N; i++) {
      if (ISNA(X[i])){
        out[i] = NA_REAL;
      } else {
        double edelta = days[i] - days[prev];
        if (edelta < 0) Rf_error("days argument is not increasing");
        double W = exp(-edelta/n);
        out[i] = W*out[prev] + (1.0 - W)*X[i];
        prev = i;
      }
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::doubles c_ema_lin(cpp11::doubles X, cpp11::doubles days, double n) {
  // linear interpolation
  // 3.3 in VSR/docs/ts_alg.pdf and  http://oroboro.com/irregular-ema/

  if (n == 0) {
    return X;
  }

  size_t N = X.size();
  cpp11::writable::doubles out(N);

  if (N > 0) {
    size_t prev = 0;
    while (ISNA(X[prev]) && prev < N){
      prev++;
      out[prev] = X[prev];
    }
    out[prev] = X[prev];

    for(size_t i = prev + 1; i < N; i++){
      if (ISNA(X[i])) {
        out[i] = NA_REAL;
      } else {
        double a = (days[i] - days[prev])/n;
        double W = exp(-a);
        double V = (1 - W)/a;
        out[i] = (W * out[prev]) + ((V - W) * X[prev]) + ((1.0 - V) * X[i]);
        prev = i;
      }
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::doubles c_ediversity(cpp11::integers X, size_t N, double n) {
  // X: ints from 1 to N inclusively
  // days: increasing vector of timestamps (in days)
  vector<double> holder(N, 0); // use 1 indexed version
  size_t len = X.size();
  cpp11::writable::doubles out(len);

  // if (len != days.size())
  //   Rf_error("size of 'X' and 'days' must aggree");

  double edelta = exp( - 1/n);

  if (len > 0 && N > 0) {
    holder[X[0] - 1] = 1.0;
    out[0] = 1.0;

    for (size_t i = 1; i < len; i++) {
      double sum = 0.0;
      // this counfounds duration and diversity, so use edelta exp( - alpha ) instead
      // double edelta = exp( -alpha * (days[i] - days[i - 1]) );
      size_t xi = X[i] - 1;
      if (xi < 0 || xi >= N )
        Rf_error("X must hold indexes from 1 to %d (N)", N);

      for (size_t j = 0; j < N; j++) {
        if (j != xi) {
          // everything else except current object decays
          holder[j] = holder[j] * edelta;
          sum = sum + holder[j];
        }
      }
      // current one is reset to 1
      holder[xi] = 1.0;
      out[i] = sum + 1.0;
    }
  }
  return out;
}

[[cpp11::register]]
cpp11::integers c_cum_unique_count(cpp11::integers X, size_t N) {
  // Count cumulatively number of different elements in X
  // N is a total number of distinct elements in X
  // X contains integer numbers in [1, N]

  vector<int> holder(N, 0);
  size_t len = X.size(), acc = 1;
  cpp11::writable::integers out(len);

  if (len > 0 && N > 0){

    out[0] = acc;

    for (size_t i = 1; i < len; i++) {

      size_t xi = X[i] - 1;
      if (xi < 0 || xi >=  N )
        Rf_error("X must hold indexes from 1 to %d (accessing %d at i=%d)", N, X[i], i);

      if( holder[X[i] - 1] < 1 ){
        holder[X[i] - 1] = 1;
        out[i] = ++acc;
      } else {
        out[i] = acc;
      }
    }

  }

  return out;
}
