#include <algorithm>
#include <cpp11.hpp>
#include <unordered_set>
#include <vector>

using namespace cpp11;
using namespace std;

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
