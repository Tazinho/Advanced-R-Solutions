
# Subsetting 

## Data types

1.  __<span style="color:red">Q</span>__: Fix each of the following common data frame subsetting errors:

    
    ```r
    mtcars[mtcars$cyl = 4, ]       # = -> ==
    mtcars[-1:4, ]                 # -1:4 -> -(1:4)
    mtcars[mtcars$cyl <= 5]        # "," is missing
    mtcars[mtcars$cyl == 4 | 6, ]  # 6 -> mtcars$cyl == 6
    ```
    

2.  __<span style="color:red">Q</span>__: Why does `x <- 1:5; x[NA]` yield five missing values? (Hint: why is 
    it different from `x[NA_real_]`?)  
    __<span style="color:green">A</span>__: `NA` is of class logical, so `x[NA]` becomes recycled to `x[NA, NA, NA, NA, NA]`. Since subsetting an atomic with `NA` leads to an `NA`, you will get 5 of them returned.
    
3.  __<span style="color:red">Q</span>__: What does `upper.tri()` return? How does subsetting a matrix with it 
    work? Do we need any additional subsetting rules to describe its behaviour?

    
    ```r
    x <- outer(1:5, 1:5, FUN = "*")
    x[upper.tri(x)]
    ```
    
    __<span style="color:green">A</span>__: `upper.tri()` has really intuitive source code. It coerces it's input to a matrix and returns a logical matrix. Hence describing it's behaviour for the use of subsetting is based on everything that applies to subsetting with logical matrices.

4.  __<span style="color:red">Q</span>__: Why does `mtcars[1:20]` return an error? How does it differ from the 
    similar `mtcars[1:20, ]`?  
    __<span style="color:green">A</span>__: In the first case `mtcar` is subsetted with a vector and the statement should return a data.frame of the first 20 columns in `mtcars`. Since `mtcars` only has 11 columns, the index is out of bounds, which explains the error. The biggest difference of `mtcars[1:20, ]` to the former case, is that now `mtcars` is subsetted with two vectors. In this case you will get returned the first 20 rows and all columns (like subsetting a matrix). 

5.  __<span style="color:red">Q</span>__: Implement your own function that extracts the diagonal entries from a
    matrix (it should behave like `diag(x)` where `x` is a matrix).  
    __<span style="color:green">A</span>__: First we copy the relevant part of the source code from the `diag()` function:

    
    ```r
    function (x = 1, nrow, ncol){
      if (is.matrix(x)) {
        if (nargs() > 1L) # this and the next line will be dropped
          stop("'nrow' or 'ncol' cannot be specified when 'x' is a matrix") 
        if ((m <- min(dim(x))) == 0L) 
          return(vector(typeof(x), 0L))
        y <- x[1 + 0L:(m - 1L) * (dim(x)[1L] + 1)]
        nms <- dimnames(x)
        if (is.list(nms) && !any(sapply(nms, is.null)) && identical((nm <- nms[[1L]][seq_len(m)]),
                                                                    nms[[2L]][seq_len(m)]))
          names(y) <- nm
        return(y)
      }
      }
    ```
  
    In the next step we drop the unncessary `nrow` and `ncol` argument and the related code in the 3rd and     4th line:

    
    ```r
    diag_v <- function (x = 1) {
      if (is.matrix(x)) {
        if ((m <- min(dim(x))) == 0L) 
          return(vector(typeof(x), 0L))
        y <- x[1 + 0L:(m - 1L) * (dim(x)[1L] + 1)] # subsetting with a vector
        nms <- dimnames(x)
        if (is.list(nms) && !any(sapply(nms, is.null)) && identical((nm <- nms[[1L]][seq_len(m)]),
                                                                    nms[[2L]][seq_len(m)]))
          names(y) <- nm
        return(y)
      }
      }
    ```
    
    If we look for the idea to capture the diagonal elements, we can see that the input matrix is subsetted with a vector, so we called this function `diag_v()`. Of course we can implement our own function `diag_m()`, where we subset with a matrix.
    
    
    ```r
    diag_m <- function (x = 1) {
      if (is.matrix(x)) {
        if ((m <- min(dim(x))) == 0L) 
          return(vector(typeof(x), 0L))
        y <- x[matrix(c(1:m,1:m), m)] # subsetting with a matrix
        nms <- dimnames(x)
        if (is.list(nms) && !any(sapply(nms, is.null)) && identical((nm <- nms[[1L]][seq_len(m)]), 
                                                                        nms[[2L]][seq_len(m)])) 
          names(y) <- nm
        return(y)
      }
    }
    ```
    
    Now we can check if we get the same results as the original function and also     compare the speed. Therefore we convert the relatively large `diamonds` dataset from the `ggplot2` package into a matrix.
    
    
    ```r
    diamonds_m <- as.matrix(ggplot2::diamonds)
    
    stopifnot(all.equal(diag(diamonds_m),
                        diag_v(diamonds_m),
                        diag_m(diamonds_m))) # our functions succeed the little test
    
    microbenchmark::microbenchmark(
      diag(diamonds_m),
      diag_v(diamonds_m),
      diag_m(diamonds_m)
    )
    #> Unit: microseconds
    #>                expr    min     lq     mean  median     uq    max neval cld
    #>    diag(diamonds_m) 12.463 13.930 17.41222 14.6630 19.245 74.413   100 a  
    #>  diag_v(diamonds_m) 15.762 17.229 20.16875 17.9620 22.361 35.190   100  b 
    #>  diag_m(diamonds_m) 17.230 19.062 22.50013 20.1615 25.293 58.284   100   c
    ```
    
    The original function seems to be a little bit faster than the trimmed and our matrix version. Maybe this is due to compiling issues
    
    
    ```r
    diag_c <- compiler::cmpfun(diag)
    diag_v_c <- compiler::cmpfun(diag_v)
    diag_m_c <- compiler::cmpfun(diag_m)
    
    # Now we can make a fair comparison of the speed:
    
    microbenchmark::microbenchmark(
      diag_c(diamonds_m),
      diag_v_c(diamonds_m),
      diag_m_c(diamonds_m)
    )
    #> Unit: microseconds
    #>                  expr    min      lq     mean median     uq    max neval
    #>    diag_c(diamonds_m) 12.830 13.7465 18.24799 14.663 19.428 67.814   100
    #>  diag_v_c(diamonds_m) 12.463 13.9300 17.17394 14.297 18.695 50.952   100
    #>  diag_m_c(diamonds_m) 14.663 15.9460 20.86527 16.863 21.994 91.641   100
    #>  cld
    #>   ab
    #>   a 
    #>    b
    ```
    
    We can see that our diag_m version is only a little bit slower than the 
    original version. However the source code of the matrix version could be a bit
    easier to read.
    
    We could also take an idea from the source code of `upper.tri()` and subset with a logical vector (but it turns out to be really slow):
    
    
    ```r
    diag_lv <- function (x = 1) {
      if (is.matrix(x)) {
        if ((m <- min(dim(x))) == 0L) 
          return(vector(typeof(x), 0L))
        y <- x[row(x) == col(x)]
        nms <- dimnames(x)
        if (is.list(nms) && !any(sapply(nms, is.null)) && identical((nm <- nms[[1L]][seq_len(m)]), 
                                                                nms[[2L]][seq_len(m)])) 
          names(y) <- nm
        return(y)
      }
      }
    ```
    
    compile it and compare it with the other versions  
    
    
    ```r
    diag_lv_c <- compiler::cmpfun(diag_lv)
    stopifnot(all.equal(diag(diamonds_m),
                        diag_v_c(diamonds_m),
                        diag_m_c(diamonds_m),
                        diag_lv_c(diamonds_m))) # our functions succeed the little test
    
    microbenchmark::microbenchmark(
      diag(diamonds_m),
      diag_v_c(diamonds_m),
      diag_m_c(diamonds_m),
      diag_lv_c(diamonds_m)
    )
    #> Unit: microseconds
    #>                   expr      min       lq       mean    median        uq
    #>       diag(diamonds_m)   13.197   15.946   22.50009   19.9785   26.2100
    #>   diag_v_c(diamonds_m)   13.197   16.495   23.43476   20.3450   28.0420
    #>   diag_m_c(diamonds_m)   15.763   19.978   28.99928   28.5930   36.6565
    #>  diag_lv_c(diamonds_m) 2821.058 3104.593 7445.82549 5293.1495 5681.7040
    #>        max neval cld
    #>     83.944   100  a 
    #>     57.918   100  a 
    #>     59.017   100  a 
    #>  97868.262   100   b
    ```

6.  __<span style="color:red">Q</span>__: What does `df[is.na(df)] <- 0` do? How does it work?  
__<span style="color:green">A</span>__: It replaces all `NA`s within `df` with the value
`0`. `is.na(df)` returns a logical matrix which is used to subset df. Since you can combine subsetting and assignment, only the matched part of `df` (the `NA`s) is replaced with `0` entries.

## Subsetting operators

1.  __<span style="color:red">Q</span>__: Given a linear model, e.g., `mod <- lm(mpg ~ wt, data = mtcars)`, extract
    the residual degrees of freedom. Extract the R squared from the model
    summary (`summary(mod)`)  
    __<span style="color:green">A</span>__: Since `mod` is of type list we can expect several possibilities:
    
    
    ```r
    mod$df.residual       # preserving output
    mod$df.r              # preserving output with partial matching
    mod["df.residual"]    # list output (without partial matching)
    mod[["df.residual"]]  # preserving output (without partial matching)
    ```
    
    The same states for `summary(mod)`, so we can use for example:
    
    
    ```r
    summary(mod)$r.squared
    ```
    
    (To get tidy output from r-models in general also the `broom` package is a good alternative).
    
## Applications

1.  __<span style="color:red">Q</span>__: How would you randomly permute the columns of a data frame? (This is an
    important technique in random forests.) Can you simultaneously permute 
    the rows and columns in one step?  
    __<span style="color:green">A</span>__: Combine `` `[` `` with the `sample()` function:
    
    
    ```r
    iris[sample(ncol(iris))] # permute rows
    iris[sample(nrow(iris)), sample(ncol(iris)), drop = FALSE] # permute both at the same time
    ```

2.  __<span style="color:red">Q</span>__: How would you select a random sample of `m` rows from a data frame? 
    What if the sample had to be contiguous (i.e., with an initial row, a 
    final row, and every row in between)?  
    __<span style="color:green">A</span>__: For example
    
    
    ```r
    m=10
    iris[sample(nrow(iris), m),]
    
    # Blockversion
    start <- sample(nrow(iris) - m + 1, 1)
    end <- start + m - 1
    iris[start:end, , drop = FALSE]
    ```
    
3.  __<span style="color:red">Q</span>__: How could you put the columns in a data frame in alphabetical order?  
__<span style="color:green">A</span>__: We can sort the names and subset by name:

    
    ```r
    iris[sort(names(iris))]
    ```
