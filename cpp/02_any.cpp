#include <cpp11.hpp>

using namespace cpp11;

[[cpp11::register]] logicals any_cpp_(logicals x, bool na_rm) {
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
