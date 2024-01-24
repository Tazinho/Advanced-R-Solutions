#include <cpp11.hpp>
#include <unordered_set>

using namespace cpp11;
using namespace std;

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
