
#include "iroll.hpp"
#include "Rinternals.h"
#include "cpp11/doubles.hpp"

template <typename FT>
SEXP inline roll_T2T(const FT &fn, SEXP X, cpp11::doubles ix,
                     SEXP left, SEXP right,
                     bool left_open, bool right_open,
                     SEXP fill) {
  double left_bound = Rf_asReal(left);
  double right_bound = Rf_asReal(right);
  if (Rf_isReal(X)) {
    return roll<cpp11::writable::doubles, cpp11::doubles>(
      fn, X, ix, left_bound, right_bound, left_open, right_open,
      Rf_asReal(fill));
  } else if (Rf_isInteger(X)) {
    return roll<cpp11::writable::integers, cpp11::integers>(
      fn, X, ix, left_bound, right_bound, left_open, right_open,
      Rf_asInteger(fill));
  } else if (Rf_isLogical(X)) {
    // https://github.com/r-lib/cpp11/issues/263
    cpp11::integers X2(Rf_coerceVector(X, INTSXP));
    return roll<cpp11::writable::logicals, cpp11::integers>(
      fn, X2, ix, left_bound, right_bound, left_open, right_open,
      Rf_asLogical(fill));
  } else {
    cpp11::stop("Invalid X argument. Must be numeric, integer or logical");
  }
}

template <typename FT>
cpp11::doubles inline roll_T2DBL(const FT &fn, SEXP X, cpp11::doubles ix,
                                 SEXP left, SEXP right,
                                 bool left_open, bool right_open,
                                 SEXP fill) {
  double left_bound = Rf_asReal(left);
  double right_bound = Rf_asReal(right);
  double fl = Rf_asReal(fill);
  if (Rf_isReal(X)) {
    return roll<cpp11::writable::doubles, cpp11::doubles>(
      fn, X, ix, left_bound, right_bound, left_open, right_open, fl);
  } else if (Rf_isInteger(X)) {
    return roll<cpp11::writable::doubles, cpp11::integers>(
      fn, X, ix, left_bound, right_bound, left_open, right_open, fl);
  } else if (Rf_isLogical(X)) {
    // https://github.com/r-lib/cpp11/issues/263
    cpp11::integers X2(Rf_coerceVector(X, INTSXP));
    return roll<cpp11::writable::doubles, cpp11::integers>(
      fn, X2, ix, left_bound, right_bound, left_open, right_open, fl);
  } else {
    cpp11::stop("Invalid X argument. Must be numeric, integer or logical");
  }
}

[[cpp11::register]]
SEXP c_roll_min(SEXP X, cpp11::doubles ix,
                SEXP left, SEXP right,
                bool left_open, bool right_open,
                SEXP fill) {
  return roll_T2T(vec_min{}, X, ix, left, right, left_open, right_open, fill);
};


[[cpp11::register]]
SEXP c_roll_max(SEXP X, cpp11::doubles ix,
                SEXP left, SEXP right,
                bool left_open, bool right_open,
                SEXP fill) {
  return roll_T2T(vec_max{}, X, ix, left, right, left_open, right_open, fill);
};

[[cpp11::register]] SEXP c_roll_first(SEXP X, cpp11::doubles ix,
                                    SEXP left, SEXP right,
                                    bool left_open, bool right_open,
                                    SEXP fill) {
  return roll_T2T(vec_first{}, X, ix, left, right, left_open, right_open, fill);
};

[[cpp11::register]] SEXP c_roll_last(SEXP X, cpp11::doubles ix,
                                      SEXP left, SEXP right,
                                      bool left_open, bool right_open,
                                      SEXP fill) {
  return roll_T2T(vec_last{}, X, ix, left, right, left_open, right_open, fill);
};

[[cpp11::register]] cpp11::doubles
c_roll_mean(SEXP X, cpp11::doubles ix, SEXP left, SEXP right,
            bool left_open, bool right_open, SEXP fill) {
  return roll_T2DBL(vec_mean{}, X, ix, left, right, left_open, right_open, fill);
}

[[cpp11::register]] cpp11::doubles
c_roll_sd(SEXP X, cpp11::doubles ix, SEXP left, SEXP right,
            bool left_open, bool right_open, SEXP fill) {
  return roll_T2DBL(vec_sd{}, X, ix, left, right, left_open, right_open, fill);
}

[[cpp11::register]] cpp11::doubles
c_roll_sum(SEXP X, cpp11::doubles ix,
           SEXP left, SEXP right,
           bool left_open, bool right_open, SEXP fill) {
  return roll_T2DBL(vec_sum{}, X, ix, left, right, left_open, right_open, fill);
}

[[cpp11::register]] cpp11::doubles
c_roll_prod(SEXP X, cpp11::doubles ix, SEXP left, SEXP right,
           bool left_open, bool right_open, SEXP fill) {
  return roll_T2DBL(vec_prod{}, X, ix, left, right, left_open,
                    right_open, fill);
}

[[cpp11::register]] cpp11::doubles
c_roll_quantile(SEXP X, cpp11::doubles ix,
                SEXP left, SEXP right,
                bool left_open, bool right_open,
                double prob, SEXP fill) {
  return roll_T2DBL(vec_quantile{prob}, X, ix, left, right, left_open, right_open, fill);
}

// UTILS
// like base cummin but ignores NAs
[[cpp11::register]] cpp11::doubles
c_cummin(const cpp11::doubles X) {
  cpp11::writable::doubles out(X.size());
  size_t N = X.size();
  if (N > 0) {
    double accum = X[0];
    out[0] = accum;
    for (int i = 1; i < N; i++) {
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
cpp11::doubles c_cummax(const cpp11::doubles X) {
  cpp11::writable::doubles out(X.size());
  size_t N = X.size();
  if (N) {
    double accum = X[0];
    out[0] = accum;
    for (int i = 1; i < N; i++) {
      double x = X[i];
      if (!ISNA(x) && x != NA_INTEGER) {
        accum = std::max(x, accum);
      }
      out[i] = accum;
    }
  }
  return out;
}
