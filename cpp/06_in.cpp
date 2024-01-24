#include <cpp11.hpp>
#include <unordered_set>

using namespace cpp11;
using namespace std;

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
