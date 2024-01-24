#include <cpp11.hpp>
#include <unordered_set>

using namespace cpp11;
using namespace std;

[[cpp11::register]] doubles unique_cpp_(const doubles& x) {
  unordered_set<double> seen;
  int n = x.size();

  writable::doubles out;
  for (int i = 0; i < n; ++i) {
    if (seen.insert(x[i]).second) out.push_back(x[i]);
  }

  return out;
}
