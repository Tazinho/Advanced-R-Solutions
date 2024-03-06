#include <algorithm>
#include <cpp11.hpp>
#include <vector>

using namespace cpp11;
using namespace std;

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
