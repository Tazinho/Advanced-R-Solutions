#include <cpp11.hpp>

using namespace cpp11;

[[cpp11::register]] doubles cumsum_cpp_(doubles x, bool na_rm = false) {
  int n = x.size();

  writable::doubles out(n);
  out[0] = x[0];

  if (na_rm == true) {
    for (int i = 1; i < n; ++i) {
      double y1 = out[i - 1], y2 = x[i];
      if (ISNAN(y2)) {
        out[i] = y1;
      } else {
        if (ISNAN(y1)) {
          out[i] = y2;
        } else {
          out[i] = y1 + y2;
        }
      }
    }
  } else {
    for (int i = 1; i < n; ++i) {
      double y1 = out[i - 1], y2 = x[i];
      if (ISNAN(y2)) {
        out[i] = NA_REAL;
      } else {
        if (ISNAN(y1)) {
          out[i] = NA_REAL;
        } else {
          out[i] = y1 + y2;
        }
      }
    }
  }

  return out;
}
