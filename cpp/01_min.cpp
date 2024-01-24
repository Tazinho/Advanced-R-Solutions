#include <cpp11.hpp>

using namespace cpp11;

[[cpp11::register]] doubles min_cpp_(cpp11::doubles x, bool na_rm) {
  int n = x.size();
  writable::doubles out = {R_PosInf};

  if (na_rm) {
    for (int i = 0; i < n; ++i) {
      if (x[i] == NA_REAL) {
        continue;
      }
      if (x[i] < out[0]) {
        out[0] = x[i];
      }
    }
  } else {
    for (int i = 0; i < n; ++i) {
      if (is_na(x[i])) {
        out[0] = NA_REAL;
        return out;
      }
      if (x[i] < out[0]) {
        out[0] = x[i];
      }
    }
  }

  return out;
}
