
# Exceptions and debugging

## Condition handling

1.  __<span style="color:red">Q</span>__: Compare the following two implementations of `message2error()`. What is the
    main advantage of `withCallingHandlers()` in this scenario? (Hint: look
    carefully at the traceback.)

    
    ```r
    message2error <- function(code) {
      withCallingHandlers(code, message = function(e) stop(e))
    }
    message2error <- function(code) {
      tryCatch(code, message = function(e) stop(e))
    }
    ```
    
    __<span style="color:green">A</span>__: 

## Defensive programming

1.  __<span style="color:red">Q</span>__: The goal of the `col_means()` function defined below is to compute the means
    of all numeric columns in a data frame.

    
    ```r
    col_means <- function(df) {
      numeric <- sapply(df, is.numeric)
      numeric_cols <- df[, numeric]
    
      data.frame(lapply(numeric_cols, mean))
    }
    ```

    However, the function is not robust to unusual inputs. Look at
    the following results, decide which ones are incorrect, and modify
    `col_means()` to be more robust. (Hint: there are two function calls
    in `col_means()` that are particularly prone to problems.)

    
    ```r
    col_means(mtcars)
    col_means(mtcars[, 0])
    col_means(mtcars[0, ])
    col_means(mtcars[, "mpg", drop = F])
    col_means(1:10)
    col_means(as.matrix(mtcars))
    col_means(as.list(mtcars))
    
    mtcars2 <- mtcars
    mtcars2[-1] <- lapply(mtcars2[-1], as.character)
    col_means(mtcars2)
    ```
    
    __<span style="color:green">A</span>__: We divide the tests according their input types and      look at the results:
    
    
    ```r
    # data.frame input
    col_means(mtcars) # correct (return: 1 row numeric data.frame)
    col_means(mtcars[, 0]) # incorrect (return: error. An empty data.frame would be better)
    col_means(mtcars[0, ])[[1]] # correct (return: 1 row numeric (NaN) data.frame, 
      # which becomes an atomic after subsetting
    col_means(mtcars[, "mpg", drop = F]) # incorrect (returns complete the numeric column as a 
      # data.frame with new names)
    
    # other input
    col_means(1:10) # correct (specific error)
    col_means(as.matrix(mtcars)) # correct (specific error)
    col_means(as.list(mtcars)) # correct (specific error)
    
    # data.fame (numeric + character columns)
    mtcars2 <- mtcars
    mtcars2[-1] <- lapply(mtcars2[-1], as.character)
    col_means(mtcars2) # incorrect (returns the complete numeric column as a data.frame
      # with new names)
    ```
    
    We can make the following changes:
    
    
    ```r
    col_means2 <- function(df) {
    #  stopifnot(is.data.frame(df))
    numeric <- vapply(df, is.numeric, logical(1)) # sapply() to vapply()
    numeric_cols <- df[, numeric, drop = FALSE] # add drop = FALSE
    data.frame(lapply(numeric_cols, mean))
    }
    ```
    
    And look at the tests again:

    
    ```r
    # data.frame input
    col_means2(mtcars) # correct
    col_means2(mtcars[, 0]) # correct
    col_means2(mtcars[0, ])[[1]] # correct
    col_means2(mtcars[, "mpg", drop = F]) #correct
    
    # other input
    col_means2(1:10) # correct (specific error)
    col_means2(as.matrix(mtcars)) # correct (specific error)
    col_means2(as.list(mtcars)) # correct (specific error)
    
    # data.frame (numeric + character columns)
    col_means2(mtcars2) # correct
    ```


    For faster failing in the error cases (where the input is not a data.frame), we can add
    `stopifnot(is.data.frame(df))` in the first line of `col_means()`.
    
2.  __<span style="color:red">Q</span>__: The following function "lags" a vector, returning a version of `x` that is `n`
    values behind the original. Improve the function so that it (1) returns a
    useful error message if `n` is not a vector, and (2) has reasonable behaviour
    when `n` is 0 or longer than `x`.

    
    ```r
    lag <- function(x, n = 1L) {
      xlen <- length(x)
      c(rep(NA, n), x[seq_len(xlen - n)])
    }
    ```
    
    __<span style="color:green">A</span>__: First we test `lag()`'s actual bahaviour:

    
    ```r
    v <- 1:3
    lag(v, 1) # -> NA 1 2  ; correct
    lag(v, 0) # -> 1 2 3   ; correct
    lag(v, 3) # -> NA NA NA; correct
    lag(v, 4) # -> Error in seq_len(xlen - n) : 
      # argument must be coercible to non-negative integer
      # in my opinion the result should be NA NA NA
    lag(v, iris) # -> Error in lag(v, iris) : (list) object cannot be coerced to type 'integer'
    ```
    
    We can adjust `lag()`, to get better bahaviour in the latter two error cases:

    
    ```r
    lag2 <- function(x, n = 1L) {
      # we make sure that a double or integer is supplied
      if (!is.numeric(n)) stop("n is not a numeric vector") 
      xlen <- length(x)
      # and we change n so that xlen - n will become 0 in case of n > length(x)
      # to get the desired behaviour for lag(v, 4)
      n <- min(xlen, n)
      c(rep(NA, n), x[seq_len(xlen - n)])
    }
    ```

    
    Now we look again at the tests:
    
    
    ```r
    lag2(v, 1) # -> correct
    lag2(v, 0) # -> correct
    lag2(v, 3) # -> correct
    lag2(v, 4) # -> NA NA NA; correct
    lag2(v, iris) # -> Error in lag2(v, iris) : n is not a numeric vector
    ```

    
    Note that we didn't test/specify `lag()`'s behaviour for negative, decimal or `length(n) != 1`     inputs of `n`.
