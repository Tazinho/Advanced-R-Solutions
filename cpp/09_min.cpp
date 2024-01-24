#include <cpp11.hpp>

using namespace cpp11;
using namespace std;

[[cpp11::register]] double min_cpp_(const doubles& x) {
  int n = x.size();
  double out = x[0];

  for (int i = 0; i < n; i++) {
    out = min(out, x[i]);
  }

  return out;
}
