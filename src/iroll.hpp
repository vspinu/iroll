#pragma once

#include "R_ext/Arith.h"
#include <algorithm>
#include <cmath>
#include <forward_list>
#include <functional>
#include <iostream>
#include <limits>
#include <vector>

#include <cpp11.hpp>

using namespace std::placeholders;

// roll FUN on window starting at current-left_bound and ending at
// current_date-right_bound.
template <typename TO, typename TX, typename TF,
          typename T = typename TO::value_type>
TO roll(const TF &fn, TX X, cpp11::doubles ix, double left_bound,
        double right_bound, bool left_open, bool right_open, T fill) {
  if (ix.size() != X.size()) {
    Rf_error("Length of 'IX' and 'X' vectors differ.");
  }
  if (right_bound < left_bound) {
    Rf_error("`left_bound` is larger than the `right_bound`");
  }

  size_t N = ix.size();
  size_t Nm1 = N - 1;
  std::forward_list<T> out_vals;

  // ref, start and end indexes
  size_t ri = 0; // ref index
  size_t si = 0; // start index, ix of the first element in the window queue
  size_t ei = 1; // end index, ix of the last element in the window

  T val;

  while (ri < N) {

    double rd = ix[ri];         // ref date
    double sd = rd + left_bound;  // start date (of the window)
    double ed = rd + right_bound; // end date (of the window)

    // move ri to next date;
    ri++;
    int nsame = 1;
    while (ri < N && rd == ix[ri]) {
      ri++;
      nsame++;
    }

    // Make interval sd <= date <= ed
    if (left_open)
      while (si < Nm1 & ix[si] <= sd)
        si++;
    else
      while (si < Nm1 & ix[si] < sd)
        si++;

    if (right_open)
      while (ei < N & ix[ei] < ed)
        ei++;
    else
      while (ei < N & ix[ei] <= ed)
        ei++;

    bool is_outside = ei <= si ||
                      (right_open ? ix[si] >= ed : ix[si] > ed) ||
                      (left_open ? ix[ei - 1] <= sd : ix[ei - 1] < sd);

    if (is_outside) {
      val = fill;
    } else {
      val = fn(si, ei, X);
    }

    /* std::cout << "ri:" << ri << " rd:" << rd << " sd:" << sd << " ed:" << ed */
    /*           << " si:" << si << " ei:" << ei - 1 << " X[ei]:" << X[ei - 1] */
    /*           << " val:" << val */
    /*           << std::endl; */

    while (nsame > 0) {
      nsame--;
      out_vals.push_front(val);
    }
  }

  /* Rf_PrintValue(X); */

  TO out = TO(N);
  for (size_t i = N; i > 0;) {
    out[--i] = out_vals.front();
    out_vals.pop_front();
  }

  return out;
}

struct vec_min {
  template <typename C, typename T = typename C::value_type>
  T operator()(size_t si, size_t ei, const C X) const {
    T accum = cpp11::na<T>();
    for (size_t i = si; i < ei; i++) {
      T x = X[i];
      if (cpp11::is_na(accum))
        accum = x;
      else
        if (!cpp11::is_na(x))
          accum = std::min(accum, x);
    }
    return accum;
  }
};

struct vec_max {
  template <typename C, typename T = typename C::value_type>
  T operator()(size_t si, size_t ei, const C X) const {
    T accum = cpp11::na<T>();
    for (size_t i = si; i < ei; i++) {
      T x = X[i];
      if (cpp11::is_na(accum))
        accum = x;
      else
        if (!cpp11::is_na(x))
          accum = std::max(accum, x);
    }
    return accum;
  }
};

struct vec_mean {
  template <typename C, typename T = typename C::value_type>
  double operator()(size_t si, size_t ei, const C X) const {
    double accum = 0;
    size_t nr = 0;
    for (int i = si; i < ei; i++) {
      // this NA check doesn't work on integers
      if (!cpp11::is_na<T>(X[i])) {
        accum += X[i];
        nr++;
      }
    }
    if (nr)
      return accum / nr;
    else
      return NA_REAL;
  }
};

struct vec_sd {
  template <typename C, typename T = typename C::value_type>
  double operator()(size_t si, size_t ei, const C X) const {
    double mean = vec_mean{}(si, ei, X);
    if (cpp11::is_na(mean))
      return (NA_REAL);
    double accum = 0.0;
    size_t nr = 0;
    for (size_t i = si; i < ei; i++) {
      if (!cpp11::is_na<T>(X[i])) {
        accum += std::pow(X[i] - mean, 2.0);
        nr++;
      }
    }
    if (nr)
      return std::sqrt(accum/nr);
    else
      return NA_REAL;
  }
};

struct vec_sum {
  template <typename C, typename T = typename C::value_type>
  T operator () (size_t si, size_t ei, C X) const {
    T accum = 0;
    for (size_t i = si; i < ei; i++) {
      T x = X[i];
      if (cpp11::is_na<T>(accum))
        accum = x;
      else if (!cpp11::is_na(x)) {
        accum += x;
      }
    }
    return accum;
  }
};

struct vec_prod {
  template <typename C, typename T = typename C::value_type>
  T operator () (size_t si, size_t ei, C X) const {
    T accum = cpp11::na<T>();
    for (size_t i = si; i < ei; i++) {
      T x = X[i];
      if (cpp11::is_na<T>(accum))
        accum = x;
      else if (!cpp11::is_na(x)) {
        accum *= x;
      }
    }
    return accum;
  }
};

struct vec_first {
  template <typename C, typename T = typename C::value_type>
  T operator()(size_t si, size_t ei, C X) const {
    return X[si];
  }
};

struct vec_last {
  template <typename C, typename T = typename C::value_type>
  T operator()(size_t si, size_t ei, C X) const {
    return X[ei - 1];
  }
};

// QUANTILES

inline double quantile(const std::vector<double> &z, double prob) {
  int n = z.size();
  if (n == 1) {
    return z[0];
  } else {
    int lo = floor((n - 1) * prob);
    int hi = lo + 1;
    return z[lo] * prob + z[hi] * (1 - prob);
  }
}

struct vec_quantile {

  double prob = .5;
  vec_quantile(double prob): prob(prob) {};

  template <typename C, typename T = typename C::value_type>
  double operator()(size_t si, size_t ei, const C X) const {
    /* printf("se: %d %d\n", si, ei); */
    const auto &first = X.begin() + si;
    const auto &last = X.begin() + ei;
    std::vector<double> z(first, last);
    /* printf("len: %d\n", (int) z.size()); */
    /* printf("sevals: %f %f\n", z.front(), z.back()); */
    sort(z.begin(), z.end());
    return quantile(z, prob);
  }
};
