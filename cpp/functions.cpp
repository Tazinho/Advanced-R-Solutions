// used to test the functions for the examples in the C++ chapter
// I run this with cpp11::source_cpp("cpp/all_funs.cpp")

#include <algorithm>
#include <cpp11.hpp>
#include <iterator>
#include <unordered_set>
#include <vector>

using namespace cpp11;
using namespace std;

[[cpp11::register]] double f1(doubles x) {
  int n = x.size();
  double y = 0;

  for (int i = 0; i < n; ++i) {
    y += x[i] / n;
  }
  return y;
}

[[cpp11::register]] doubles f2(doubles x) {
  int n = x.size();
  writable::doubles out(n);

  out[0] = x[0];
  for (int i = 1; i < n; ++i) {
    out[i] = out[i - 1] + x[i];
  }
  return out;
}

[[cpp11::register]] bool f3(logicals x) {
  int n = x.size();

  for (int i = 0; i < n; ++i) {
    if (x[i]) return true;
  }
  return false;
}

[[cpp11::register]] int f4(function pred, list x) {
  int n = x.size();

  for (int i = 0; i < n; ++i) {
    logicals res = as_cpp<logicals>(pred(x[i]));
    if (res[0]) return i + 1;
  }
  return 0;
}

// [[cpp11::register]] doubles f5(doubles x, doubles y) {
//   int n = std::max(x.size(), y.size());

//   writable::doubles x1(n);
//   writable::doubles y1(n);
//   for (int i = 0; i < n; ++i) {
//     x1[i] = x[i % x.size()];
//     y1[i] = y[i % y.size()];
//   }

//   writable::doubles out(n);

//   for (int i = 0; i < n; ++i) {
//     double a = x1[i];
//     double b = y1[i];
//     out[i] = std::min(a, b);
//   }

//   return out;
// }

[[cpp11::register]] doubles f5(doubles x, doubles y) {
  int n = std::max(x.size(), y.size());

  vector<double> x1(n);
  vector<double> y1(n);
  for (int i = 0; i < n; ++i) {
    x1[i] = x[i % x.size()];
    y1[i] = y[i % y.size()];
  }

  writable::doubles out(n);

  for (int i = 0; i < n; ++i) {
    out[i] = std::min(x1[i], y1[i]);
  }

  return out;
}

[[cpp11::register]] bool all_cpp_(logicals x) {
  int n = x.size();

  for (int i = 0; i < n; ++i) {
    if (!x[i]) return false;
  }
  return true;
}

[[cpp11::register]] doubles cumprod_cpp_(doubles x) {
  int n = x.size();
  writable::doubles out(n);

  out[0] = x[0];
  for (int i = 1; i < n; ++i) {
    out[i] = out[i - 1] * x[i];
  }
  return out;
}

[[cpp11::register]] doubles cummin_cpp_(doubles x) {
  int n = x.size();
  writable::doubles out(n);

  out[0] = x[0];
  for (int i = 1; i < n; ++i) {
    double x1 = out[i - 1];
    double x2 = x[i];
    out[i] = std::min(x1, x2);
  }

  return out;
}

[[cpp11::register]] doubles cummax_cpp_(doubles x) {
  int n = x.size();
  writable::doubles out(n);

  out[0] = x[0];
  for (int i = 1; i < n; ++i) {
    double a = out[i - 1];
    double b = x[i];
    out[i] = std::max(a, b);
  }
  return out;
}

// [[cpp11::register]] doubles diff_cpp_(doubles x) {
//   int n = x.size();
//   writable::doubles out(n - 1);

//   for (int i = 1; i < n; i++) {
//     out[i - 1] = x[i] - x[i - 1];
//   }
//   return out;
// }

[[cpp11::register]] doubles difflag_cpp_(doubles x, int lag = 1) {
  int n = x.size();

  if (lag >= n) stop("`lag` must be less than `length(x)`.");

  writable::doubles out(n - lag);

  for (int i = lag; i < n; i++) {
    out[i - lag] = x[i] - x[i - lag];
  }
  return out;
}

[[cpp11::register]] doubles range_cpp_(doubles x) {
  double omin = x[0], omax = x[0];
  int n = x.size();

  if (n == 0) stop("`length(x)` must be greater than 0.");

  for (int i = 1; i < n; i++) {
    omin = std::min(x[i], omin);
    omax = std::max(x[i], omax);
  }

  writable::doubles out(2);
  out[0] = omin;
  out[1] = omax;
  return out;
}

[[cpp11::register]] double var_cpp_(doubles x) {
  int n = x.size();

  if (n < 2) {
    return NA_REAL;
  }

  double mx = 0;
  for (int i = 0; i < n; ++i) {
    mx += x[i] / n;
  }

  double out = 0;
  for (int i = 0; i < n; ++i) {
    out += pow(x[i] - mx, 2);
  }

  return out / (n - 1);
}

// [[cpp11::register]] doubles min_cpp_(cpp11::doubles x, bool na_rm = false) {
//   int n = x.size();
//   writable::doubles out = {R_PosInf};

//   if (na_rm) {
//     for (int i = 0; i < n; ++i) {
//       if (x[i] == NA_REAL) {
//         continue;
//       }
//       if (x[i] < out[0]) {
//         out[0] = x[i];
//       }
//     }
//   } else {
//     for (int i = 0; i < n; ++i) {
//       if (is_na(x[i])) {
//         out[0] = NA_REAL;
//         return out;
//       }
//       if (x[i] < out[0]) {
//         out[0] = x[i];
//       }
//     }
//   }

//   return out;
// }

[[cpp11::register]] logicals any_cpp_(logicals x, bool na_rm = false) {
  int n = x.size();
  writable::logicals out = {false};

  if (na_rm == false) {
    for (int i = 0; i < n; ++i) {
      if (is_na(x[i])) {
        out[0] = NA_LOGICAL;
        return out;
      } else {
        if (x[i]) {
          out[0] = true;
        }
      }
    }
  }

  if (na_rm) {
    for (int i = 0; i < n; ++i) {
      if (is_na(x[i])) {
        continue;
      }
      if (x[i]) {
        out[0] = true;
        return out;
      }
    }
  }

  return out;
}

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

[[cpp11::register]] double median_cpp_(const doubles& x) {
  int n = x.size();

  vector<double> y(n);
  for (int i = 0; i < n; ++i) {
    y[i] = x[i];
  }

  if (n % 2 == 0) {
    partial_sort(y.begin(), y.begin() + n / 2 + 1, y.end());
    return (y[n / 2 - 1] + y[n / 2]) / 2;
  } else {
    partial_sort(y.begin(), y.begin() + (n + 1) / 2, y.end());
    return y[(n + 1) / 2 - 1];
  }
}

[[cpp11::register]] logicals in_cpp_(const strings& x, const strings& table) {
  unordered_set<string> seen;
  seen.insert(table.begin(), table.end());

  int n = x.size();
  writable::logicals out(n);
  for (int i = 0; i < n; ++i) {
    out[i] = seen.find(x[i]) != seen.end();
  }

  return out;
}

[[cpp11::register]] doubles unique_cpp_(const doubles& x) {
  unordered_set<double> seen;
  int n = x.size();

  writable::doubles out;
  for (int i = 0; i < n; ++i) {
    if (seen.insert(x[i]).second) out.push_back(x[i]);
  }

  return out;
}

unordered_set<double> unique_short1_cpp_(const doubles& x) {
  return unordered_set<double>(x.begin(), x.end());
}

[[cpp11::register]] doubles unique_short2_cpp_(const doubles& x) {
  unordered_set<double> y = unique_short1_cpp_(x);

  int n = y.size();
  writable::doubles out(n);
  copy(y.begin(), y.end(), out.begin());

  return out;
}

[[cpp11::register]] double min_cpp_(const doubles& x) {
  int n = x.size();
  double out = x[0];

  for (int i = 0; i < n; i++) {
    out = min(out, x[i]);
  }

  return out;
}

[[cpp11::register]] double which_min_cpp_(const doubles& x) {
  int out = distance(x.begin(), min_element(x.begin(), x.end()));

  return out + 1;
}

[[cpp11::register]] integers union_cpp_(const integers& x, const integers& y) {
  vector<int> vx(x.begin(), x.end());
  vector<int> vy(y.begin(), y.end());

  sort(vx.begin(), vx.end());
  sort(vy.begin(), vy.end());

  vector<int> tmp(vx.size() + vy.size());

  vector<int>::iterator out_end =
      set_union(vx.begin(), vx.end(), vy.begin(), vy.end(), tmp.begin());

  int prev_value = 0;
  writable::integers out;

  for (vector<int>::iterator it = tmp.begin(); it != out_end; ++it) {
    if ((it != tmp.begin()) && (prev_value == *it)) continue;

    out.push_back(*it);

    prev_value = *it;
  }

  return out;
}

[[cpp11::register]] integers intersect_cpp_(const integers& x,
                                            const integers& y) {
  vector<int> vx(x.begin(), x.end());
  vector<int> vy(y.begin(), y.end());

  sort(vx.begin(), vx.end());
  sort(vy.begin(), vy.end());

  vector<int> tmp(min(vx.size(), vy.size()));

  vector<int>::iterator out_end =
      set_intersection(vx.begin(), vx.end(), vy.begin(), vy.end(), tmp.begin());

  writable::integers out;

  for (vector<int>::iterator it = tmp.begin(); it != out_end; ++it) {
    out.push_back(*it);
  }

  return out;
}

[[cpp11::register]] integers setdiff_cpp_(const integers& x,
                                          const integers& y) {
  vector<int> vx(x.begin(), x.end());
  vector<int> vy(y.begin(), y.end());

  sort(vx.begin(), vx.end());
  sort(vy.begin(), vy.end());

  vector<int> tmp(vx.size());

  vector<int>::iterator out_end =
      set_difference(vx.begin(), vx.end(), vy.begin(), vy.end(), tmp.begin());

  writable::integers out;

  for (vector<int>::iterator it = tmp.begin(); it != out_end; ++it) {
    out.push_back(*it);
  }

  return out;
}
