#include <cpp11.hpp>

using namespace cpp11;

[[cpp11::register]] doubles diff_cpp_(doubles x, int lag, bool na_rm) {
  int n = x.size();

  if (lag >= n) stop("`lag` must be less than `length(x)`.");

  writable::doubles out(n - lag);

  for (int i = lag; i < n; i++) {
    if (is_na(x[i]) || is_na(x[i - lag])) {
      if (!na_rm) {
        writable::doubles out_na(n - lag);
        for (int j = 0; j < n - lag; ++j) {
          out_na[j] = NA_REAL;
        }
        return out_na;
      }
      out[i - lag] = NA_REAL;
      continue;
    }
    out[i - lag] = x[i] - x[i - lag];
  }

  return out;
}
