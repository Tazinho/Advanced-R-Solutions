#include <algorithm>
#include <cpp11.hpp>
#include <iterator>

using namespace cpp11;
using namespace std;

[[cpp11::register]] double which_min_cpp_(const doubles& x) {
  int out = distance(x.begin(), min_element(x.begin(), x.end())
  );

  return out + 1;
}
