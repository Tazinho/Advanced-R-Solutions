
# Rcpp

## Getting started with C++

1.  __<span style="color:red">Q</span>__: With the basics of C++ in hand, it's now a great time to practice by reading and writing some simple C++ functions. For each of the following functions, read the code and figure out what the corresponding base R function is. You might not understand every part of the code yet, but you should be able to figure out the basics of what the function does.

    ```cpp
    double f1(NumericVector x) {
      int n = x.size();
      double y = 0;
    
      for(int i = 0; i < n; ++i) {
        y += x[i] / n;
      }
      return y;
    }
    
    NumericVector f2(NumericVector x) {
      int n = x.size();
      NumericVector out(n);
    
      out[0] = x[0];
      for(int i = 1; i < n; ++i) {
        out[i] = out[i - 1] + x[i];
      }
      return out;
    }
    
    bool f3(LogicalVector x) {
      int n = x.size();
    
      for(int i = 0; i < n; ++i) {
        if (x[i]) return true;
      }
      return false;
    }
    
    int f4(Function pred, List x) {
      int n = x.size();
    
      for(int i = 0; i < n; ++i) {
        LogicalVector res = pred(x[i]);
        if (res[0]) return i + 1;
      }
      return 0;
    }
    
    NumericVector f5(NumericVector x, NumericVector y) {
      int n = std::max(x.size(), y.size());
      NumericVector x1 = rep_len(x, n);
      NumericVector y1 = rep_len(y, n);
    
      NumericVector out(n);
    
      for (int i = 0; i < n; ++i) {
        out[i] = std::min(x1[i], y1[i]);
      }
    
      return out;
    }
    ```

    __<span style="color:green">A</span>__:  The R equivalents are:  

    *   f1: `mean()`  
    *   f2: `cumsum()`  
    *   f3: `any()`
    *   f4: `Position()`
    *   f5: `pmin()`


1.  __<span style="color:red">Q</span>__:  To practice your function writing skills, convert the following functions into C++. For now, assume the inputs have no missing values.  
  
    1. `all()`
  
    
    ```r
    cppFunction('
    bool allC(LogicalVector x) {
    int n = x.size();
    
    for(int i = 0; i < n; ++i) {
      if (!x[i]) return false;
    }
    return true;
    }')
    ```
  
    2. `cumprod()`, `cummin()`, `cummax()`.
  
    
    ```r
    NumericVector cumprodC(NumericVector x) {
    int n = x.size();
    NumericVector out(n);
    
    out[0] = x[0];
    for(int i = 1; i < n; ++i) {
      out[i]  = out[i - 1] * x[i];
    }
    return out;
    }')
    
    cppFunction('
    NumericVector cumminC(NumericVector x) {
    
    int n = x.size();
    NumericVector out(n);
    
    out[0] = x[0];
    for(int i = 1; i < n; ++i) {
      out[i]  = std::min(out[i - 1], x[i]);
    }
    return out;
    }')
    
    cppFunction('
    NumericVector cummaxC(NumericVector x) {
    int n = x.size();
    NumericVector out(n);
    
    out[0] = x[0];
    for(int i = 1; i < n; ++i) {
      out[i]  = std::max(out[i - 1], x[i]);
    }
    return out;
    }')
    ```

    3. `diff()`. Start by assuming lag 1, and then generalise for lag `n`.  
    
    
    ```r
    cppFunction('
    NumericVector diffC(NumericVector x){
    int n = x.size();
    NumericVector out(n - 1);
    
    for(int i = 1; i < n; i++){
        out[i - 1] = x[i] - x[i - 1];
    }
    return out ;
    }')
    
    cppFunction('
    NumericVector difflagC(NumericVector x, int lag){
    int n = x.size();
    NumericVector out(n - lag);
            
    for(int i = lag; i < n; i++){
        out[i - lag] = x[i] - x[i - lag];
    }
    return out;
    }')
    ```
  
    4. `range`.  
    
    
    ```r
    cppFunction('NumericVector rangeC(NumericVector x){
    double omin, omax;  
    int n = x.size();
    NumericVector out(2);
    
    omin = x[0];
    omax = x[0];
    
    for(int i = 1; i < n; i++){
        omin = std::min(x[i], omin);
        omax = std::max(x[i], omax);
    }
    
    out[0] = omin;
    out[1] = omax;
    return out;
    }')
    ```
  
    5. `var`. Read about the approaches you can take on 
     [wikipedia](http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance).
    Whenever implementing a numerical algorithm, it's always good to check what 
    is already known about the problem.

## Missing values

1. Rewrite any of the functions from the first exercise to deal with missing 
   values. If `na.rm` is true, ignore the missing values. If `na.rm` is false, 
   return a missing value if the input contains any missing values. Some 
   good functions to practice with are `min()`, `max()`, `range()`, `mean()`, 
   and `var()`.  
   
    
    ```r
    cppFunction('NumericVector minC(NumericVector x, bool narm){
    int n = x.size();
    LogicalVector index = is_na(x);
    NumericVector omin(1);
    bool na_check = false;
    bool na_check_all = true;
    
    for(int i; i<n; i++){
        if (index[i]) na_check = true;
    }
    
    for(int i; i<n; i++){
        if (!index[i]) na_check_all = false;
    }
    
    if (narm) {
        for(int i = n-1; i >= 0; i--){
            if (!index[i]) omin[0] = x[i];
        }
      
        for(int i = 1; i < n; i++) {
            if (!index[i]) omin[0] = std::min(x[i], omin[0]);
        }
      
        if (na_check_all) {
            omin[0] = NA_REAL;
        }
    } else if (na_check) {
        omin[0] = NA_REAL;
    } else {
        omin[0] = x[0];
        for(int i = 1; i < n; i++){
            omin = std::min(x[i], omin[0]);
        }
    }
    
    return omin;
    }')
    
    cppFunction('NumericVector maxC(NumericVector x, bool narm){
    int n = x.size();
    LogicalVector index = is_na(x);
    NumericVector omax(1);
    bool na_check = false;
    bool na_check_all = true;
    
    for(int i; i<n; i++){
        if (index[i]) na_check = true;
    }
    
    for(int i; i<n; i++){
        if (!index[i]) na_check_all = false;
    }
    
    if (narm) {
        for(int i = n-1; i >= 0; i--){
            if (!index[i]) omax[0] = x[i];
        }
      
        for(int i = 1; i < n; i++) {
            if (!index[i]) omax[0] = std::max(x[i], omax[0]);
        }
      
        if (na_check_all) {
            omax[0] = NA_REAL;
        }
    } else if (na_check) {
        omax[0] = NA_REAL;
    } else {
        omax[0] = x[0];
        for(int i = 1; i < n; i++){
            omax = std::max(x[i], omax[0]);
        }
    }
    
    return omax;
    }')
    
    cppFunction('NumericVector rangeC(NumericVector x, bool narm){
    int n = x.size();
    LogicalVector index = is_na(x);
    NumericVector out(2);
    NumericVector omin(1);
    NumericVector omax(1);
    bool na_check = false;
    bool na_check_all = true;
    
    for(int i; i<n; i++){
    if (index[i]) na_check = true;
    }
    
    for(int i; i<n; i++){
    if (!index[i]) na_check_all = false;
    }
      
    /* Minimum */
    if (narm) {
    for(int i = n-1; i >= 0; i--){
    if (!index[i]) omin[0] = x[i];
    }
            
    for(int i = 1; i < n; i++) {
    if (!index[i]) omin[0] = std::min(x[i], omin[0]);
    }
            
    if (na_check_all) {
    omin[0] = NA_REAL;
    }
    } else if (na_check) {
    omin[0] = NA_REAL;
    } else {
    omin[0] = x[0];
    for(int i = 1; i < n; i++){
    omin = std::min(x[i], omin[0]);
    }
    }
            
    /* Maximum */
    if (narm) {
    for(int i = n-1; i >= 0; i--){
    if (!index[i]) omax[0] = x[i];
    }
            
    for(int i = 1; i < n; i++) {
    if (!index[i]) omax[0] = std::max(x[i], omax[0]);
    }
            
    if (na_check_all) {
    omax[0] = NA_REAL;
    }
    } else if (na_check) {
    omax[0] = NA_REAL;
    } else {
    omax[0] = x[0];
    for(int i = 1; i < n; i++){
    omax = std::max(x[i], omax[0]);
    }
    }
            
    out[0] = omin[0];
    out[1] = omax[0];
    return out;
    }')
    
    cppFunction('NumericVector meanC(NumericVector x, bool narm){
    int n = x.size();
    LogicalVector index = is_na(x);
    bool na_check = false;
    bool na_check_all = true;
    int n_corrected = 0;
    NumericVector out(1);
            
    for(int i; i<n; i++){
        if (index[i]) na_check = true;
    }
            
    for(int i; i<n; i++){
        if (!index[i]) na_check_all = false;
    }
    
    for(int i; i<n; i++){
        if (!index[i]) n_corrected++;
    }
    
    /* narm = T */
    if (narm){
        if (na_check_all) {
            out[0] = NA_REAL;
        }  else {
            out[0] = 0;
            for(int i = 0; i < n; ++i) {
                if (!index[i]) out[0] += x[i] / n_corrected;
            }
        }
    }
    
    /* narm = F */
    if (!narm){
        if (na_check) {
            out[0] = NA_REAL;
        } else {
            for(int i = 0; i < n; ++i) {
                out[0] += x[i] / n;
              }
        }
    }
    
    return out;
    }')
    ```

1. Rewrite `cumsum()` and `diff()` so they can handle missing values. Note that 
   these functions have slightly more complicated behaviour.  
   
    
    ```r
    cppFunction('NumericVector cumsumC(NumericVector x) {
    int n = x.size();
    NumericVector out(n);
    LogicalVector index = is_na(x);
    
    out[0] = x[0];
    for(int i = 1; i < n; ++i) {
      if (index[i - 1]) {
        out[i] = NA_REAL;
    } else{
        out[i] = out[i - 1] + x[i];
      }
    }
    
    return out;
    }')
    
    cppFunction('
    NumericVector difflagC(NumericVector x, int lag){
    int n = x.size();
    NumericVector out(n - lag);
    LogicalVector index = is_na(x);
            
    for(int i = lag; i < n; i++){
        if ((index[i]) || (index[i - lag])) {
            out[i - lag] = NA_REAL;
        } else {
            out[i - lag] = x[i] - x[i - lag];
        }
    }
    return out;
    }')
    ```

## The STL

To practice using the STL algorithms and data structures, implement the following using R functions in C++, using the hints provided:

1. `median.default()` using `partial_sort`.

    
    ```r
    #include <algorithm>
    #include <Rcpp.h>
    using namespace Rcpp;
    
    // [[Rcpp::export]]
    double medianC(NumericVector x) {
      int n = x.size();
      double out;
      if (n % 2 == 0){
        std::partial_sort (x.begin(), x.begin() + n / 2 + 1, x.end());
        out = (x[n / 2 - 1] + x[n / 2]) / 2;
      } else {
        std::partial_sort (x.begin(), x.begin() + (n + 1) / 2, x.end());
        out = x[(n + 1) / 2 - 1];
      }
      
      return out;
    }
    ```

1. `%in%` using `unordered_set` and the `find()` or `count()` methods.

1. `unique()` using an `unordered_set` (challenge: do it in one line!).  

    
    ```r
    // [[Rcpp::plugins(cpp11)]]
    #include <Rcpp.h>
    #include <unordered_set>
    using namespace Rcpp;
    
    // [[Rcpp::export]]
    NumericVector uniqueC(NumericVector x) {
    std::unordered_set<int> seen;
    int n = x.size();
    LogicalVector dup(n);
    std::vector<double> out;
    
    for (int i = 0; i < n; ++i) {
      dup[i] = !seen.insert(x[i]).second;
      if (!dup[i]) {
          out.push_back(x[i]);
      }
    }
    
    return wrap(out);
    }
    ```

1. `min()` using `std::min()`, or `max()` using `std::max()`.

    
    ```r
    #include <Rcpp.h>
    using namespace Rcpp;
    
    // [[Rcpp::export]]
    double minC(NumericVector x){
      int n = x.size();
      double out;
      
      for (int i = 0; i < n; i++){
        out = std::min(out, x[i]);
      }
      
      return out;
    }
    ```

1. `which.min()` using `min_element`, or `which.max()` using `max_element`.  

    
    ```r
    #include <Rcpp.h>
    #include <algorithm>
    #include <iterator>
    
    using namespace Rcpp;
    
    // [[Rcpp::export]]
    double which_minC(NumericVector x){
      int out;
      out = std::distance(x.begin(),std::min_element(x.begin(),x.end()));
      out++;
      
      return out;
    }
    ```

1. `setdiff()`, `union()`, and `intersect()` for integers using sorted ranges 
   and `set_union`, `set_intersection` and `set_difference`.
