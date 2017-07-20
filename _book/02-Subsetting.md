
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
    __<span style="color:green">A</span>__: `NA` is of class logical, so `x[NA]` becomes recycled to `x[NA, NA, NA, NA, NA]`. Since subsetting an atomic with `NA` leads to an `NA`, you will get 5 of them returned. (Note that the recycling won't happen, if you subset with `NA_real_`, `NA_integer`, `NA_character` or `NA_complex`. In fact the letter gives an error).
    
3.  __<span style="color:red">Q</span>__: What does `upper.tri()` return? How does subsetting a matrix with it 
    work? Do we need any additional subsetting rules to describe its behaviour?

    
    ```r
    x <- outer(1:5, 1:5, FUN = "*")
    x[upper.tri(x)]
    ```
    
    __<span style="color:green">A</span>__: `upper.tri()` has really intuitive source code. It coerces its input to a matrix and returns a logical matrix. Hence describing it's behaviour for the use of subsetting is based on everything that applies to subsetting with logical matrices.

4.  __<span style="color:red">Q</span>__: Why does `mtcars[1:20]` return an error? How does it differ from the 
    similar `mtcars[1:20, ]`?  
    __<span style="color:green">A</span>__: In the first case `mtcars` is subsetted with a vector and the statement should return a data.frame of the first 20 columns in `mtcars` (this is like listsubsetting in the sense, that each element is a column and the result is typestable). Since `mtcars` only has 11 columns, the index is out of bounds, which explains the error. The biggest difference of `mtcars[1:20, ]` to the former case, is that now `mtcars` is subsetted with two vectors. In this case you will get returned the first 20 rows and all columns (like subsetting a matrix). 

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
    #>                expr    min     lq      mean median     uq      max neval
    #>    diag(diamonds_m) 12.464 13.929  15.81398 14.297 15.029  129.763   100
    #>  diag_v(diamonds_m) 12.830 13.563  14.48710 14.296 14.846   24.927   100
    #>  diag_m(diamonds_m) 14.663 15.762 115.80829 16.496 17.229 9880.293   100
    #>  cld
    #>    a
    #>    a
    #>    a
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
    #>                  expr    min     lq     mean median     uq     max neval
    #>    diag_c(diamonds_m) 12.830 14.297 18.54854 15.029 19.612 112.535   100
    #>  diag_v_c(diamonds_m) 13.196 14.113 18.09037 14.663 20.162  67.081   100
    #>  diag_m_c(diamonds_m) 14.663 16.129 19.19374 16.863 21.811  42.888   100
    #>  cld
    #>    a
    #>    a
    #>    a
    ```
    
    We can see that our `diag_m()` version is only a little bit slower than the 
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
    #>                   expr      min        lq       mean    median        uq
    #>       diag(diamonds_m)   13.930   15.7625   23.07928   20.3445   27.6760
    #>   diag_v_c(diamonds_m)   13.564   15.3960   21.13282   18.6960   25.1100
    #>   diag_m_c(diamonds_m)   15.763   17.2290   26.56892   27.3090   33.1745
    #>  diag_lv_c(diamonds_m) 2481.621 2648.5890 3949.52209 3815.1700 5025.0055
    #>       max neval cld
    #>    94.940   100  a 
    #>    46.554   100  a 
    #>    51.319   100  a 
    #>  6952.568   100   b
    ```

6.  __<span style="color:red">Q</span>__: What does `df[is.na(df)] <- 0` do? How does it work?  
__<span style="color:green">A</span>__: It replaces all `NA`s within `df` with the value
`0`. `is.na(df)` returns a logical matrix which is used to subset df. Since you can combine subsetting and assignment (specifically `[<-` is called), only the matched part of `df` (the `NA`s) is replaced with `0` entries.

## Subsetting operators

1.  __<span style="color:red">Q</span>__: Given a linear model, e.g., `mod <- lm(mpg ~ wt, data = mtcars)`, extract
    the residual degrees of freedom. Extract the R squared from the model
    summary (`summary(mod)`)  
    __<span style="color:green">A</span>__: Since `mod` is of type list we can expect several possibilities:
    
    
    ```r
    mod$df.residual       # simplifying output
    mod$df.r              # simplifying output with partial matching
    mod["df.residual"]    # preserving list output (without partial matching)
    mod[["df.residual"]]  # simplifying output (without partial matching)
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
    iris[sample(ncol(iris))] # permute columns
    iris[sample(nrow(iris)), sample(ncol(iris)), drop = FALSE] # permute both at the same time
    ```

2.  __<span style="color:red">Q</span>__: How would you select a random sample of `m` rows from a data frame? 
    What if the sample had to be contiguous (i.e., with an initial row, a 
    final row, and every row in between)?  
    __<span style="color:green">A</span>__: For example
    
    
    ```r
    m=10
    iris[sample(nrow(iris), m), , drop = FALSE]
    
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
