

# Functionals

## My first functional: lapply()

1.  __<span style="color:red">Q</span>__: Why are the following two invocations of `lapply()` equivalent?

    
    ```r
    trims <- c(0, 0.1, 0.2, 0.5)
    x <- rcauchy(100)
    
    lapply(trims, function(trim) mean(x, trim = trim))
    lapply(trims, mean, x = x)
    ```
    
    __<span style="color:green">A</span>__: In the first statement each element of `trims` is explicitly supplied to `mean()`'s second argument. In the latter statement this happens via 
positional matching, since `mean()`'s first argument is supplied via name
in `lapply()`'s third argument (`...`).

2.  __<span style="color:red">Q</span>__: The function below scales a vector so it falls in the range [0, 1]. How
    would you apply it to every column of a data frame? How would you apply it 
    to every numeric column in a data frame?

    
    ```r
    scale01 <- function(x) {
      rng <- range(x, na.rm = TRUE)
      (x - rng[1]) / (rng[2] - rng[1])
    }
    ```
    
    __<span style="color:green">A</span>__: Since this function needs numeric input, one can check this via an if clause. If one also wants to return non-numeric input columns, these can be supplied to the `else` argument of the `if()` "function":
    
    
    ```r
    data.frame(lapply(iris, function(x) if (is.numeric(x)) scale01(x) else x))
    ```

3.  __<span style="color:red">Q</span>__: Use both for loops and `lapply()` to fit linear models to the
    `mtcars` using the formulas stored in this list:

    
    ```r
    formulas <- list(
      mpg ~ disp,
      mpg ~ I(1 / disp),
      mpg ~ disp + wt,
      mpg ~ I(1 / disp) + wt
    )
    ```
    
    __<span style="color:green">A</span>__: Like in the first exercise, we can create two `lapply()` versions:
    
    
    ```r
    # lapply (2 versions)
    la1 <- lapply(formulas, lm, data = mtcars)
    la2 <- lapply(formulas, function(x) lm(formula = x, data = mtcars))
    
    # for loop
    lf1 <- vector("list", length(formulas))
    for (i in seq_along(formulas)){
      lf1[[i]] <- lm(formulas[[i]], data = mtcars)
    }
    ```
    
    Note that all versions return the same content, but they won't be identical, since the values of the "call" element will differ between each version.
    
4.  __<span style="color:red">Q</span>__: Fit the model `mpg ~ disp` to each of the bootstrap replicates of `mtcars` 
    in the list below by using a for loop and `lapply()`. Can you do it 
    without an anonymous function?

    
    ```r
    bootstraps <- lapply(1:10, function(i) {
      rows <- sample(1:nrow(mtcars), rep = TRUE)
      mtcars[rows, ]
    })
    ```
    
    __<span style="color:green">A</span>__: 
    
    
    ```r
    # lapply without anonymous function
    la <- lapply(bootstraps, lm, formula = mpg ~ disp)
    
    # for loop
    lf <- vector("list", length(bootstraps))
    for (i in seq_along(bootstraps)){
      lf[[i]] <- lm(mpg ~ disp, data = bootstraps[[i]])
    }
    ```

5.  __<span style="color:red">Q</span>__: For each model in the previous two exercises, extract $R^2$ using the
    function below.

    
    ```r
    rsq <- function(mod) summary(mod)$r.squared
    ```
    
    __<span style="color:green">A</span>__: For the models in exercise 3:
    
    
    ```r
    sapply(la1, rsq)
    #> [1] 0.7183433 0.8596865 0.7809306 0.8838038
    sapply(la2, rsq)
    #> [1] 0.7183433 0.8596865 0.7809306 0.8838038
    sapply(lf1, rsq)
    #> [1] 0.7183433 0.8596865 0.7809306 0.8838038
    ```
    
    And the models in exercise 4:
    
    
    ```r
    sapply(la, rsq)
    #>  [1] 0.7207897 0.6324568 0.7241623 0.7637960 0.7478840 0.6720768 0.6811906
    #>  [8] 0.6742183 0.7644756 0.7440796
    sapply(lf, rsq)
    #>  [1] 0.7207897 0.6324568 0.7241623 0.7637960 0.7478840 0.6720768 0.6811906
    #>  [8] 0.6742183 0.7644756 0.7440796
    ```

## For loops functionals: friends of lapply():

1.  __<span style="color:red">Q</span>__: Use `vapply()` to:
    
    a) Compute the standard deviation of every column in a numeric data frame.
    
    a) Compute the standard deviation of every numeric column in a mixed data
       frame. (Hint: you'll need to use `vapply()` twice.)
       
    __<span style="color:green">A</span>__: As a numeric `data.frame` we choose `cars`:
    
    
    ```r
    vapply(cars, sd, numeric(1))
    ```
    
    And as a mixed `data.frame` we choose `iris`:

    
    ```r
    vapply(iris[vapply(iris, is.numeric, logical(1))],
           sd, 
           numeric(1))
    ```

2.  __<span style="color:red">Q</span>__: Why is using `sapply()` to get the `class()` of each element in 
    a data frame dangerous?
    
    __<span style="color:green">A</span>__: Columns of data.frames might have more than one class, so the class of `sapply()`'s output may differ from time to time (silently). If ...
    
    * all columns have one class: `sapply()` returns a character vector
    * one column has more classes than the others: `sapply()` returns a list
    * all columns have the same number of classes, which is more than one: `sapply()` returns a matrix
    
    For example:
    
    
    ```r
    a <- letters[1:3]
    class(a) <- c("class1", "class2")
    df <- data.frame(a = character(3))
    df$a <- a
    df$b <- a
    class(sapply(df, class))
    #> [1] "matrix"
    ```
    
    Note that this case often appears, wile working with the POSIXt types, POSIXct and POSIXlt.
    
3.  __<span style="color:red">Q</span>__: The following code simulates the performance of a t-test for non-normal 
    data. Use `sapply()` and an anonymous function to extract the p-value from 
    every trial.

    
    ```r
    trials <- replicate(
      100, 
      t.test(rpois(10, 10), rpois(7, 10)),
      simplify = FALSE
    )
    ```
    
    Extra challenge: get rid of the anonymous function by using `[[` directly.
    
    __<span style="color:green">A</span>__: 
    
    
    ```r
    # anonymous function:
    sapply(trials, function(x) x[["p.value"]])
    # without anonymous function:
    sapply(trials, "[[", "p.value")
    ```

4.  __<span style="color:red">Q</span>__: What does `replicate()` do? What sort of for loop does it eliminate? Why 
    do its arguments differ from `lapply()` and friends?
    
    __<span style="color:green">A</span>__: As stated in `?replicate`:
    
    > replicate is a wrapper for the common use of sapply for repeated evaluation of an expression (which will usually involve random number generation).
    
    We can see this clearly in the source code:
    
    
    ```
    #> function (n, expr, simplify = "array") 
    #> sapply(integer(n), eval.parent(substitute(function(...) expr)), 
    #>     simplify = simplify)
    #> <bytecode: 0x00000000175406f0>
    #> <environment: namespace:base>
    ```
    
    Like `sapply()` `replicate()` eliminates a for loop. As explained for `Map()` in the textbook, also every `replicate()` could have been written via `lapply()`. But using `replicate()` is more concise, and more clearly indicates what you're trying to do.

5.  __<span style="color:red">Q</span>__: Implement a version of `lapply()` that supplies `FUN` with both the name 
    and the value of each component.
    
    __<span style="color:green">A</span>__:
    
    
    ```r
    lapply_nms <- function(X, FUN, ...){
      Map(FUN, X, names(X), ...)
    }
    lapply_nms(iris, function(x, y) c(class(x), y))
    #> $Sepal.Length
    #> [1] "numeric"      "Sepal.Length"
    #> 
    #> $Sepal.Width
    #> [1] "numeric"     "Sepal.Width"
    #> 
    #> $Petal.Length
    #> [1] "numeric"      "Petal.Length"
    #> 
    #> $Petal.Width
    #> [1] "numeric"     "Petal.Width"
    #> 
    #> $Species
    #> [1] "factor"  "Species"
    ```

6.  __<span style="color:red">Q</span>__: Implement a combination of `Map()` and `vapply()` to create an `lapply()`
    variant that iterates in parallel over all of its inputs and stores its 
    outputs in a vector (or a matrix). What arguments should the function 
    take?
    
    __<span style="color:green">A</span>__ As we understand this exercise, it is about working with a list of lists, like in the following example:
    
    
    ```r
    testlist <- list(iris, mtcars, cars)
    lapply(testlist, function(x) vapply(x, mean, numeric(1)))
    #> Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    #> returning NA
    #> [[1]]
    #> Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
    #>     5.843333     3.057333     3.758000     1.199333           NA 
    #> 
    #> [[2]]
    #>        mpg        cyl       disp         hp       drat         wt 
    #>  20.090625   6.187500 230.721875 146.687500   3.596563   3.217250 
    #>       qsec         vs         am       gear       carb 
    #>  17.848750   0.437500   0.406250   3.687500   2.812500 
    #> 
    #> [[3]]
    #> speed  dist 
    #> 15.40 42.98
    ```
    
    So we can get the same result with a more specialized function:
    
    
    ```r
    lmapply <- function(X, FUN, FUN.VALUE, simplify = FALSE){
      out <- Map(function(x) vapply(x, FUN, FUN.VALUE), X)
      if(simplify == TRUE){return(simplify2array(out))}
      out
    }
    
    lmapply(testlist, mean, numeric(1))
    #> Warning in mean.default(X[[i]], ...): argument is not numeric or logical:
    #> returning NA
    #> [[1]]
    #> Sepal.Length  Sepal.Width Petal.Length  Petal.Width      Species 
    #>     5.843333     3.057333     3.758000     1.199333           NA 
    #> 
    #> [[2]]
    #>        mpg        cyl       disp         hp       drat         wt 
    #>  20.090625   6.187500 230.721875 146.687500   3.596563   3.217250 
    #>       qsec         vs         am       gear       carb 
    #>  17.848750   0.437500   0.406250   3.687500   2.812500 
    #> 
    #> [[3]]
    #> speed  dist 
    #> 15.40 42.98
    ```

7.  __<span style="color:red">Q</span>__: Implement `mcsapply()`, a multicore version of `sapply()`. Can you
    implement `mcvapply()`, a parallel version of `vapply()`? Why or why not?

## Manipulating matrices and data frames

1.  __<span style="color:red">Q</span>__: How does `apply()` arrange the output? Read the documentation and perform 
    some experiments.
    
    __<span style="color:green">A</span>__:
    
    `apply()` arranges its output columns (or list elements) according to the order of the margin.
The rows are ordered by the other dimensions, starting with the "last" dimension
of the input object. What this means should become clear by looking at the three and four dimensional cases of the following example:

    
    ```r
    # for two dimensional cases everything is sorted by the other dimension
    arr2 <- array(1:9, dim = c(3, 3), dimnames = list(paste0("row", 1:3),
                                                  paste0("col", 1:3)))
    arr2
    apply(arr2, 1, head, 1) # Margin is row
    apply(arr2, 1, head, 9) # sorts by col
    
    apply(arr2, 2, head, 1) # Margin is col
    apply(arr2, 2, head, 9) # sorts by row
    
    # 3 dimensional
    arr3 <- array(1:27, dim = c(3,3,3), dimnames = list(paste0("row", 1:3),
                                                    paste0("col", 1:3),
                                                    paste0("time", 1:3)))
    arr3
    apply(arr3, 1, head, 1) # Margin is row
    apply(arr3, 1, head, 27) # sorts by time and col
    
    apply(arr3, 2, head, 1) # Margin is col
    apply(arr3, 2, head, 27) # sorts by time and row
    
    apply(arr3, 3, head, 1) # Margin is time
    apply(arr3, 3, head, 27) # sorts by col and row
    
    # 4 dimensional
    arr4 <- array(1:81, dim = c(3,3,3,3), dimnames = list(paste0("row", 1:3),
                                                      paste0("col", 1:3),
                                                      paste0("time", 1:3),
                                                      paste0("var", 1:3)))
    arr4
    
    apply(arr4, 1, head, 1) # Margin is row
    apply(arr4, 1, head, 81) # sorts by var, time, col
    
    apply(arr4, 2, head, 1) # Margin is col
    apply(arr4, 2, head, 81) # sorts by var, time, row
    
    apply(arr4, 3, head, 1) # Margin is time
    apply(arr4, 3, head, 81) # sorts by var, col, row
    
    apply(arr4, 4, head, 1) # Margin is var
    apply(arr4, 4, head, 81) # sorts by time, col, row
    ```

2.  __<span style="color:red">Q</span>__: There's no equivalent to `split()` + `vapply()`. Should there be? When 
    would it be useful? Implement one yourself.
    
    __<span style="color:green">A</span>__: We can modify the `tapply2()` approach from the book, where `split()` and `sapply()` were combined:
    
    
    ```r
    v_tapply <- function(x, group, f, FUN.VALUE, ..., USE.NAMES = TRUE) {
      pieces <- split(x, group)
      vapply(pieces, f, FUN.VALUE, ..., USE.NAMES = TRUE)
    }
    ```
    
    `tapply()` has a `SIMPLIFY` argument. When you set it to `FALSE`, `tapply()` will always return a list. It is easy to create cases where the length and the types/classes of the list elements vary depending on the input. The `vapply()` version could be useful, if you want to control the structure of the output to get an error according to some logic of a specific usecase or you want typestable output to build up other functions on top of it.
    
3.  __<span style="color:red">Q</span>__: Implement a pure R version of `split()`. (Hint: use `unique()` and 
    subsetting.) Can you do it without a for loop?
    
    __<span style="color:green">A</span>__: 
    
    
    ```r
    split2 <- function(x, f, drop = FALSE, ...){
      # there are three relevant cases for f. f is a character, f is a factor and all
      # levels occur, f is a factor and some levels don't occur.
      
      # first we check if f is a factor
      fact <- is.factor(f)
      
      # if drop it set to TRUE, we drop the non occuring levels.
      # (If f is a character, this has no effect.)
      if(drop){f <- f[, drop = TRUE]}
      
      # now we want all unique elements/levels of f
      levs <- if (fact) {unique(levels(f))} else {as.character(unique(f))}
      
      # we use these levels to subset x and supply names for the resulting output.
      setNames(lapply(levs, function(lv) x[f == lv, , drop = FALSE]), levs)
    }
    ```

4.  __<span style="color:red">Q</span>__: What other types of input and output are missing? Brainstorm before you look up some answers in the [plyr paper](http://www.jstatsoft.org/v40/i01/).
    
    __<span style="color:green">A</span>__: From the suggested plyr paper, we can extract a lot of possible combinations and list them up on a table. Sean C. Anderson already has done this based on a presentation from Hadley Wickham and provided the following result [here](http://seananderson.ca/2013/12/01/plyr.html). 
    
    | object type        | array       | data frame   | list        | nothing   |
    |--------------------|-------------|--------------|-------------|-----------|
    | array              | `apply`     | `.`          | `.`         | `.`       |
    | data frame         | `.`         | `aggregate`  | `by`        | `.`       |
    | list               | `sapply`    | `.`          | `lapply`    | `.`       |
    | n replicates       | `replicate` | `.`          | `replicate` | `.`       |
    | function arguments | `mapply`    | `.`          | `mapply`    | `.`       |
    
    Note the column nothing, which is specifically for usecases, where sideeffects like plotting or writing data are intended.
    
## Manipulating lists

1.  __<span style="color:red">Q</span>__: Why isn't `is.na()` a predicate function? What base R function is closest
    to being a predicate version of `is.na()`?
    
    __<span style="color:green">A</span>__: Because a predicate function always returns `TRUE` or `FALSE`. `is.na(NULL)` returns `logical(0)`, which excludes it from being a predicate function. The closest in base that we are aware of is `anyNA()`, if one applies it elementwise.

2.  __<span style="color:red">Q</span>__: Use `Filter()` and `vapply()` to create a function that applies a summary 
    statistic to every numeric column in a data frame.
    
    __<span style="color:green">A</span>__: 
    
    
    ```r
    vapply_num <- function(X, FUN, FUN.VALUE){
      vapply(Filter(is.numeric, X), FUN, FUN.VALUE)
    }
    ```

3.  __<span style="color:red">Q</span>__: What's the relationship between `which()` and `Position()`? What's
    the relationship between `where()` and `Filter()`?
    
    __<span style="color:green">A</span>__: `which()` returns all indices of true entries from a logical vector. `Position()` returns just the first (default) or the last integer index of all true entries that occur by applying a predicate function on a vector. So the default relation is `Position(f, x) <=> min(which(f(x)))`.
    
    `where()`, defined in the book as:
    
    
    ```r
    where <- function(f, x) {
      vapply(x, f, logical(1))
    } 
    ```
    
    is useful to return a logical vector from a condition asked on elements of a list or a data frame. `Filter(f, x)` returns all elements of a list or a data frame, where
the supplied predicate function returns `TRUE`. So the relation is
`Filter(f, x) <=> x[where(f, x)]`.

4.  __<span style="color:red">Q</span>__: Implement `Any()`, a function that takes a list and a predicate function, 
    and returns `TRUE` if the predicate function returns `TRUE` for any of 
    the inputs. Implement `All()` similarly.
    
    __<span style="color:green">A</span>__: `Any()`:
    
    
    ```r
    Any <- function(l, pred){
      stopifnot(is.list(l))
      
      for (i in seq_along(l)){
        if (pred(l[[i]])) return(TRUE)
      }
      
      return(FALSE)
    }
    ```
    
    `All()`:
    
    
    ```r
    All <- function(l, pred){
      stopifnot(is.list(l))
      
      for (i in seq_along(l)){
        if (!pred(l[[i]])) return(FALSE)
      }
      
      return(TRUE)
    }
    ```

5.  __<span style="color:red">Q</span>__: Implement the `span()` function from Haskell: given a list `x` and a 
    predicate function `f`, `span` returns the location of the longest 
    sequential run of elements where the predicate is true. (Hint: you 
    might find `rle()` helpful.)
    
    __<span style="color:green">A</span>__: Our `span_r()` function returns the first index of     the longest sequential run of elements where the predicate is true. In case of more than one longest sequenital, more than one first_index is returned.
    
    
    ```r
    span_r <- function(l, pred){
      # We test if l is a list
      stopifnot(is.list(l))
    
      # we preallocate a logical vector and save the result
      # of the predicate function applied to each element of the list
      test <- vector("logical", length(l))
      for (i in seq_along(l)){
        test[i] <- (pred(l[[i]]))
      }
      # we return NA, if the output of pred is always FALSE
      if(!any(test)) return(NA_integer_)
      
      # Otherwise we look at the length encoding of TRUE and FALSE values.
      rle_test <- rle(test)
      # Since it might happen, that more than one maximum series of TRUE's appears,
      # we have to implement some logic, which might be easier, if we save the rle 
      # output in a data.frmame
      rle_test <- data.frame(lengths = rle_test[["lengths"]],
                              values = rle_test[["values"]],
                              cumsum = cumsum(rle_test[["lengths"]]))
      rle_test[["first_index"]] <- rle_test[["cumsum"]] - rle_test[["lengths"]] + 1
      # In the last line we calculated the first index in the original list for every encoding
      # In the next line we calculate a column, which gives the maximum 
      # encoding length among all encodings with the value TRUE
      rle_test[["max"]] <-  max(rle_test[rle_test[, "values"] == TRUE, ][,"lengths"])
      # Now we just have to subset for maximum length among all TRUE values and return the
      # according "first index":
      rle_test[rle_test$lengths == rle_test$max & rle_test$values == TRUE, ]$first_index
    }
    ```

## Mathematical functionals

1.  __<span style="color:red">Q</span>__: Implement `arg_max()`. It should take a function and a vector of inputs, 
    and return the elements of the input where the function returns the highest 
    value. For example, `arg_max(-10:5, function(x) x ^ 2)` should return -10.
    `arg_max(-5:5, function(x) x ^ 2)` should return `c(-5, 5)`.
    Also implement the matching `arg_min()` function.
    
    __<span style="color:green">A</span>__: `arg_max()`:
    
    
    ```r
    arg_max <- function(x, f){
      x[f(x) == max(f(x))]
    }
    ```
    
    `arg_min()`:
    
    
    ```r
    arg_min <- function(x, f){
      x[f(x) == min(f(x))]
    }
    ```

2.  __<span style="color:red">Q</span>__: Challenge: read about the 
    [fixed point algorithm](http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-12.html#%_sec_1.3). 
    Complete the exercises using R.

## A family of functions

1.  __<span style="color:red">Q</span>__: Implement `smaller` and `larger` functions that, given two inputs, return 
    either the smaller or the larger value. Implement `na.rm = TRUE`: what 
    should the identity be? (Hint: 
    `smaller(x, smaller(NA, NA, na.rm = TRUE), na.rm = TRUE)` must be `x`, so 
    `smaller(NA, NA, na.rm = TRUE)` must be bigger than any other value of x.) 
    Use `smaller` and `larger` to implement equivalents of `min()`, `max()`,
    `pmin()`, `pmax()`, and new functions `row_min()` and `row_max()`.
    
    __<span style="color:green">A</span>__: We can do almost everything as shown in the case study in the textbook. First we define the functions `smaller_()` and `larger_()`. We use the underscore suffix, to built up non suffixed versions on top, which will include the `na.rm` parameter. In contrast to the `add()` example from the book, we change two things at this step. We won't include errorchecking, since this is done later at the top level and we return `NA_integer_` if any of the arguments is `NA` (this is important, if na.rm is set to `FALSE` and wasn't needed by the `add()` example, since `+` already returns `NA` in this case.)
    
    
    ```r
    smaller_ <- function(x, y){
      if(anyNA(c(x, y))){return(NA_integer_)}
      out <- x
      if(y < x) {out <- y}
      out
    }
    
    larger_ <- function(x, y){
      if(anyNA(c(x, y))){return(NA_integer_)}
      out <- x
      if(y > x) {out <- y}
      out
    }
    ```
    
    We can take `na.rm()` from the book:
    
    
    ```r
    rm_na <- function(x, y, identity) {
      if (is.na(x) && is.na(y)) {
        identity
        } else if (is.na(x)) {
          y
          } else {
            x
          }
    }
    ```
    
    To find the identity value, we can apply the same argument as in the textbook, hence our functions are also associative and the following equation should hold:
    
    ```
    3 = smaller(smaller(3, NA), NA) = smaller(3, smaller(NA, NA)) = 3
    ```
    
    So the identidy has to be greater than 3. When we generalize from 3 to any real number this means that the identity has to be greater than any number, which leads us to infinity. Hence identity has to be `Inf` for `smaller()` (and `-Inf` for `larger()`), which we implement next:
    
    
    ```r
    smaller <- function(x, y, na.rm = FALSE) {
      stopifnot(length(x) == 1, length(y) == 1, is.numeric(x) | is.logical(x),
                is.numeric(y) | is.logical(y))
      if (na.rm && (is.na(x) || is.na(y))) rm_na(x, y, Inf) else smaller_(x,y)
    }
    
    larger <- function(x, y, na.rm = FALSE) {
      stopifnot(length(x) == 1, length(y) == 1, is.numeric(x) | is.logical(x),
                is.numeric(y) | is.logical(y))
      if (na.rm && (is.na(x) || is.na(y))) rm_na(x, y, -Inf) else larger_(x,y)
    }
    ```
    
    Like `min()` and `max()` can act on vectors, we can implement this easyly for our new functions. As shown in the book, we also have to set the `init` parameter to the identity value.
    
    
    ```r
    r_smaller <- function(xs, na.rm = TRUE) {
      Reduce(function(x, y) smaller(x, y, na.rm = na.rm), xs, init = Inf)
    }
    # some tests
    r_smaller(c(1:3, 4:(-1)))
    #> [1] -1
    r_smaller(NA, na.rm = TRUE)
    #> [1] Inf
    r_smaller(numeric())
    #> [1] Inf
    
    r_larger <- function(xs, na.rm = TRUE) {
      Reduce(function(x, y) larger(x, y, na.rm = na.rm), xs, init = -Inf)
    }
    # some tests
    r_larger(c(1:3), c(4:1))
    #> [1] 3
    r_larger(NA, na.rm = TRUE)
    #> [1] -Inf
    r_larger(numeric())
    #> [1] -Inf
    ```
    
    We can also create vectorised versions as shown in the book. We will just show the `smaller()` case to become not too verbose.
    
    
    ```r
    v_smaller1 <- function(x, y, na.rm = FALSE){
      stopifnot(length(x) == length(y), is.numeric(x) | is.logical(x), 
                is.numeric(y)| is.logical(x))
      if (length(x) == 0) return(numeric())
      simplify2array(
        Map(function(x, y) smaller(x, y, na.rm = na.rm), x, y)
      )
    }
    
    v_smaller2 <- function(x, y, na.rm = FALSE) {
      stopifnot(length(x) == length(y), is.numeric(x) | is.logical(x), 
                is.numeric(y)| is.logical(x))
      vapply(seq_along(x), function(i) smaller(x[i], y[i], na.rm = na.rm),
             numeric(1))
    }
    
    # Both versions give the same results
    v_smaller1(1:10, c(2,1,4,3,6,5,8,7,10,9))
    #>  [1] 1 1 3 3 5 5 7 7 9 9
    v_smaller2(1:10, c(2,1,4,3,6,5,8,7,10,9))
    #>  [1] 1 1 3 3 5 5 7 7 9 9
    
    v_smaller1(numeric(), numeric())
    #> numeric(0)
    v_smaller2(numeric(), numeric())
    #> numeric(0)
    
    v_smaller1(c(1, NA), c(1, NA), na.rm = FALSE)
    #> [1]  1 NA
    v_smaller2(c(1, NA), c(1, NA), na.rm = FALSE)
    #> [1]  1 NA
    
    v_smaller1(NA,NA)
    #> [1] NA
    v_smaller2(NA,NA)
    #> [1] NA
    ```
    
    Of course, we are also able to copy paste the rest from the textbook, to solve the last part of the exercise:
    
    
    ```r
    row_min <- function(x, na.rm = FALSE) {
      apply(x, 1, r_smaller, na.rm = na.rm)
    }
    col_min <- function(x, na.rm = FALSE) {
      apply(x, 2, r_smaller, na.rm = na.rm)
    }
    arr_min <- function(x, dim, na.rm = FALSE) {
      apply(x, dim, r_smaller, na.rm = na.rm)
    }
    ```

2.  __<span style="color:red">Q</span>__: Create a table that has _and_, _or_, _add_, _multiply_, _smaller_, and 
    _larger_ in the columns and _binary operator_, _reducing variant_, 
    _vectorised variant_, and _array variants_ in the rows.

    a) Fill in the cells with the names of base R functions that perform each of
       the roles.

    a) Compare the names and arguments of the existing R functions. How
       consistent are they? How could you improve them?

    a) Complete the matrix by implementing any missing functions.
    
    __<span style="color:green">A</span>__ In the following table we can see the requested base R functions, that we are aware of:

    |            | and      | or       | add      | multiply | smaller  | larger   |
    |------------|----------|----------|----------|----------|----------|----------|
    | binary     | `&&`     | `||`     |          |          |          |          |
    | reducing   | `all`    | `any`    | `sum`    | `prod`   | `min`    | `max`    |
    | vectorised | `&`      | `|`      | `+`      | `*`      | `pmin`   | `pmax`   |
    | array      |          |          |          |          |          |          |
    
    Notice that we were relatively strict about the _binary_ row. Since the _vectorised_ and _reducing_ versions are more general, then the _binary_ versions, we could have used them twice. However, this doesn't seem to be the intention of this exercise. 
    
    The last part of this exercise can be solved via copy pasting from the book and the last exercise for the _binary_ row and creating combinations of `apply()` and the _reducing_ versions for the _array_ row. We think the array functions just need a dimension and an `rm.na` argument. We don't know how we would name them, but sth. like `sum_array(1, na.rm = TRUE)` could be ok.
    
    The second part of the exercise is hard to solve complete. But in our opinion, there are two important parts. The behaviour for special inputs like `NA`, `NaN`, `NULL` and zero length atomics should be consistent and all versions should have a `rm.na` argument, for which the functions also behave consistent. In the follwing table, we return the output of `` `f`(x, 1) ``, where `f` is the function in the first column  and `x` is the special input in the header (the named functions also have an `rm.na` argument, which is `FALSE` by default). The order of the arguments is important, because of lazy evaluation.
    
    |         | `NA`     | `NaN`    | `NULL`       | `logical(0)` | `integer(0)` |
    |---------|----------|----------|--------------|--------------|--------------|
    | `&&`    | `NA`     | `NA`     | `error`      | `NA`         | `NA`         |
    | `all`   | `NA`     | `NA`     | `TRUE`       | `TRUE`       | `TRUE`       |
    | `&`     | `NA`     | `NA`     | `error`      | `logical(0)` | `logical(0)` |
    | `||`    | `TRUE`   | `TRUE`   | `error`      | `TRUE`       | `TRUE`       |
    | `any`   | `TRUE`   | `TRUE`   | `TRUE`       | `TRUE`       | `TRUE`       |
    | `|`     | `TRUE`   | `TRUE`   | `error`      | `logical(0)` | `logical(0)` |
    | `sum`   | `NA`     | `NaN`    | `1`          | `1`          | `1`          |
    | `+`     | `NA`     | `NaN`    | `numeric(0)` | `numeric(0)` | `numeric(0)` |
    | `prod`  | `NA`     | `NaN`    | `1`          | `1`          | `1`          |
    | `*`     | `NA`     | `NaN`    | `numeric(0)` | `numeric(0)` | `numeric(0)` |
    | `min`   | `NA`     | `NaN`    | `1`          | `1`          | `1`          |
    | `pmin`  | `NA`     | `NaN`    | `numeric(0)` | `numeric(0)` | `numeric(0)` |
    | `max`   | `NA`     | `NaN`    | `1`          | `1`          | `1`          |
    | `pmax`  | `NA`     | `NaN`    | `numeric(0)` | `numeric(0)` | `numeric(0)` |
    
    We can see, that the vectorised and reduced numerical functions are all consistent. However it is not, that the first three logical functions return `NA` for `NA` and `NaN`, while the 4th till 6th function all return `TRUE`. Then `FALSE` would be more consistent for the first three or the return of `NA` for all and an extra `na.rm` argument. In seems relatively hard to find an easy rule for all cases and especially the different behaviour for `NULL` is relatively confusing. Another good opportunity for sorting the functions would be to differentiate between "numerical" and "logical" operators first and then between binary, reduced and vectorised, like below (we left the last colum, which is redundant, because of coercion, as intended):
    
    | `` `f(x,1)` `` | `NA`     | `NaN`    | `NULL`       | `logical(0)` |
    |----------------|----------|----------|--------------|--------------|
    |    `&&`        | `NA`     | `NA`     | error        | `NA`         |
    |    `||`        | `TRUE`   | `TRUE`   | error        | `TRUE`       |
    |    `all`       | `NA`     | `NA`     | `TRUE`       | `TRUE`       |
    |    `any`       | `TRUE`   | `TRUE`   | `TRUE`       | `TRUE`       |
    |    `&`         | `NA`     | `NA`     | error        | `logical(0)` |
    |    `|`         | `TRUE`   | `TRUE`   | error        | `logical(0)` |
    |    `sum`       | `NA`     | `NaN`    | 1            | 1            |
    |    `prod`      | `NA`     | `NaN`    | 1            | 1            |
    |    `min`       | `NA`     | `NaN`    | 1            | 1            |
    |    `max`       | `NA`     | `NaN`    | 1            | 1            |
    |    `+`         | `NA`     | `NaN`    | `numeric(0)` | `numeric(0)` |
    |    `*`         | `NA`     | `NaN`    | `numeric(0)` | `numeric(0)` |
    |    `pmin`      | `NA`     | `NaN`    | `numeric(0)` | `numeric(0)` |
    |    `pmax`      | `NA`     | `NaN`    | `numeric(0)` | `numeric(0)` |
    
    The other point are the naming conventions. We think they are clear, but it could be useful to provide the missing binary operators and name them for example `++`, `**`, `<>`, `><` to be consistent.

3.  __<span style="color:red">Q</span>__: How does `paste()` fit into this structure? What is the scalar binary 
    function that underlies `paste()`? What are the `sep` and `collapse` 
    arguments to `paste()` equivalent to? Are there any `paste` variants 
    that don't have existing R implementations?
    
    __<span style="color:green">A</span>__ `paste()` behaves like a mix. If you supply only length one arguments, it will behave like a reducing function, i.e. :
    
    
    ```r
    paste("a", "b", sep = "")
    #> [1] "ab"
    paste("a", "b","", sep = "") 
    #> [1] "ab"
    ```
    
    If you supply at least one element with length greater then one, it behaves like a vectorised function, i.e. :
    
    
    ```r
    paste(1:3)
    #> [1] "1" "2" "3"
    paste(1:3, 1:2)
    #> [1] "1 1" "2 2" "3 1"
    paste(1:3, 1:2, 1)
    #> [1] "1 1 1" "2 2 1" "3 1 1"
    ```
    
    We think it should be possible to implement a new `paste()` starting from
    
    
    ```r
    p_binary <- function(x, y = "") {
      stopifnot(length(x) == 1, length(y) == 1)
      paste0(x,y)
    }
    ```

    
    The `sep` argument is equivalent to bind `sep` on every `...` input supplied to `paste()`, but the last and then bind these results together. In relations:
    
    ```
    paste(n1, n2, ...,nm , sep = sep) <=>
    paste0(paste0(n1, sep), paste(n2, n3, ..., nm, sep = sep)) <=>
    paste0(paste0(n1, sep), paste0(n2, sep), ..., paste0(nn, sep), paste0(nm))
    ```
    We can check this for scalar and non scalar input
    
    
    ```r
    # scalar:
    paste("a", "b", "c", sep = "_")
    #> [1] "a_b_c"
    paste0(paste0("a", "_"), paste("b", "c", sep = "_"))
    #> [1] "a_b_c"
    paste0(paste0("a", "_"), paste0("b", "_"), paste0("c"))
    #> [1] "a_b_c"
    
    # non scalar
    paste(1:2, "b", "c", sep = "_")
    #> [1] "1_b_c" "2_b_c"
    paste0(paste0(1:2, "_"), paste("b", "c", sep = "_"))
    #> [1] "1_b_c" "2_b_c"
    paste0(paste0(1:2, "_"), paste0("b", "_"), paste0("c"))
    #> [1] "1_b_c" "2_b_c"
    ```

    
    collapse just binds the outputs for non scalar input together with the collapse input.
    In relations:
    
    ```
    for input A1, ..., An, where Ai = a1i:ami,
    
    paste(A1 , A2 , ...,  An, collapse = collapse) 
    <=>
    paste0(
          paste0(paste(  a11,   a12, ...,   a1n), collapse),
          paste0(paste(  a21,   a22, ...,   a2n), collapse),
          .................................................
          paste0(paste(am-11, am-12, ..., am-1n), collapse),      
                 paste(  am1,   am2, ...,   amn)
          )
    ```
    
    One can see this easily by intuition from examples:
    
    
    ```r
    paste(1:5, 1:5, 6, sep = "", collapse = "_x_")
    #> [1] "116_x_226_x_336_x_446_x_556"
    paste(1,2,3,4, collapse = "_x_")
    #> [1] "1 2 3 4"
    paste(1:2,1:2,2:3,3:4, collapse = "_x_")
    #> [1] "1 1 2 3_x_2 2 3 4"
    ```
    
    We think the only paste version that is not implemented in base R is an array version.
    At least we are not aware of sth. like `row_paste` or `paste_apply` etc.
