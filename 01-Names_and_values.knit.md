


# Names and values

## Binding basics

1.  __<span style="color:red">Q</span>__: Explain the relationship between `a`, `b`, `c` and `d` in the following code:

    
    ```r
    a <- 1:10
    b <- a
    c <- b
    d <- 1:10
    ```
    
    __<span style="color:green">A</span>__: `a`, `b`, `c` point to the same object (same address in memory) with the value `1:10`. So this object gets three name bindings. `d` points to another object with the same value.

1.  __<span style="color:red">Q</span>__: The following code accesses the mean function in multiple different ways. Do they all point to the same underlying function object? Verify with `lobstr::obj_addr()`.
    
    
    ```r
    mean
    base::mean
    get("mean")
    evalq(mean)
    match.fun("mean")
    ```
    
    __<span style="color:green">A</span>__: Yes, they point to the same object. To confirm that, we look at the address of the underlying function object. Since `lobstr::obj_addr()` currently returns an error, we use `pryr::address()` to inspect the addresses:
    
    
    ```r
    pryr::address(mean)
    #> [1] "0x18b21368"
    
    x1 <- mean
    pryr::address(x1)
    #> [1] "0x1e08a2f8"
    
    x2 <- base::mean
    pryr::address(x2)
    #> [1] "0x1e08a2f8"
    
    x3 <- get("mean")
    pryr::address(x3)
    #> [1] "0x1e08a2f8"
    
    x4 <- evalq(mean)
    pryr::address(x4)
    #> [1] "0x1e08a2f8"
    
    x5 <- match.fun("mean")
    pryr::address(x5)
    #> [1] "0x1e08a2f8"
    ```
    
1.  __<span style="color:red">Q</span>__: By default, base R data import functions, like `read.csv()`, will automatically convert non-syntactic names to syntactic names. Why might this be problematic? What option allows you to suppress this behaviour?
    
    __<span style="color:green">A</span>__: This might be especially problematic in non-interactive R usage, when R reads and writes data and the output is expected to contain the same names as used in the data source. One can suppress the name conversion via setting the `check.names` argument to `FALSE`.
    
1.  __<span style="color:red">Q</span>__: What rules does `make.names()` use to convert non-syntactic names into syntactic names?
    
    __<span style="color:green">A</span>__: A valid name starts with a letter or a dot (which must not be followed by a number). It also consists only of letters, numbers, dots and underscores (`"_"` are allowed since R version 1.9.0). There are three main strategies applied to construct syntactically valid names (see also `?make.names`):
    
    * prepend an `X`: This strategy is applied d to names which don't start with a letter or start with a dot followed by a number:
    
    
    ```r
    make.names("")
    #> [1] "X"
    make.names(".1")
    #> [1] "X.1"
    ```
    
    * (additionally) non valid characters are replaced by a dot:
    
    
    ```r
    make.names("@")  # prepending + . replacement 
    #> [1] "X."
    make.names("  ")  # prepending + .. replacement
    #> [1] "X.."
    make.names("non-valid")  # . replacement
    #> [1] "non.valid"
    ```
    
    * reserved R keywords (see `?reserved`) get a dot appended:
    
    
    ```r
    make.names("if")
    #> [1] "if."
    ```
    
    Also mentioned in the help file:

    > The definition of a letter depends on the current locale, but only ASCII digits are considered to be digits.

1.  __<span style="color:red">Q</span>__: I slightly simplified the rules that govern syntactic names. Why is `.123e1` not a syntactic name? Read `?make.names`.
    
    __<span style="color:green">A</span>__: It is not syntactically valid as it starts with one dot which is followed by a number.

## Copy-on-modify

1.  __<span style="color:red">Q</span>__: Why is `tracemem(1:10)` not useful?

    __<span style="color:green">A</span>__: Without a binding `1:10` will not stay in memory (there will be no reference) and it makes no sense to track an object for copies which doesn't exist. Also when we assign `1:10` to a name, it will be clear, that `1:10` will only be the value of the object created and there is no "general" object `1:10`, which one would wan't to track.

1.  __<span style="color:red">Q</span>__: Explain why `tracemem()` records two copies when you run this code. 
    Hint: carefully look at the difference between this code and the code show earlier in the section.
     
    
    ```r
    x <- 1:3
    tracemem(x)
    
    x[[3]] <- 4
    ```
    
    __<span style="color:green">A</span>__: Initially `x` is an integer vector. Within the replacement call, we assign a double to the third element of `x`. So besides the new value of the third element also a type conversion (coercion) is triggered affecting whole vector:
    
    
    ```r
    # two copies
    x <- 1:3
    tracemem(x)
    #> [1] "<000000001F5F7CD8>"
    
    x[[3]] <- 4
    #> tracemem[0x000000001f5f7cd8 -> 0x000000001f692858]: eval eval withVisible withCallingHandlers handle timing_fn evaluate_call <Anonymous> evaluate in_dir block_exec call_block process_group.block process_group withCallingHandlers process_file <Anonymous> <Anonymous> do.call eval eval eval eval eval.parent local 
    #> tracemem[0x000000001f692858 -> 0x000000001f696118]: eval eval withVisible withCallingHandlers handle timing_fn evaluate_call <Anonymous> evaluate in_dir block_exec call_block process_group.block process_group withCallingHandlers process_file <Anonymous> <Anonymous> do.call eval eval eval eval eval.parent local
    
    # the same as 
    x <- 1:3
    tracemem(x)
    #> [1] "<000000001F74F0C0>"
    
    x <- 4L
    x <- as.double(x)
    
    # one copy
    x <- 1:3
    
    tracemem(x)
    #> [1] "<000000001F90C728>"
    x[[3]] <- 4L
    #> tracemem[0x000000001f90c728 -> 0x000000001f975798]: eval eval withVisible withCallingHandlers handle timing_fn evaluate_call <Anonymous> evaluate in_dir block_exec call_block process_group.block process_group withCallingHandlers process_file <Anonymous> <Anonymous> do.call eval eval eval eval eval.parent local
    ```

1.  __<span style="color:red">Q</span>__: Sketch out the relationship between the following objects:

    
    ```r
    a <- 1:10
    b <- list(a, a)
    c <- list(b, a, 1:10)
    ```
    
    __<span style="color:green">A</span>__: `a` contains a reference to an address with the value `1:10`. `b` contains a list of the same reference as `a` (twice). `c` contains a list of `b`, `a` (both containing the same reference three times) and a reference pointing to a different address containing the same value (`1:10`).

1.  __<span style="color:red">Q</span>__: What happens when you run this code:

    
    ```r
    x <- list(1:10)
    x[[2]] <- x
    ```
    
    Draw a picture.

## Object size

1.  __<span style="color:red">Q</span>__: Take the following list. Why is its size somewhat misleading?

    
    ```r
    x <- list(mean, sd, var)
    # obj_size(x)
    #> 16,928 B
    ```

1.  __<span style="color:red">Q</span>__: Predict the output of the following code:

    
    ```r
    # x <- 1:1e6
    # obj_size(x)
    # 
    # y <- list(x, x)
    # obj_size(y)
    # obj_size(x, y)
    # 
    # y[[1]][[1]] <- 10
    # obj_size(y)
    # obj_size(x, y)
    # 
    # y[[2]][[1]] <- 10
    # obj_size(y)
    # obj_size(x, y)
    ```
    
    __<span style="color:green">A</span>__: Since `lobstr::obj_size()` currently throws an error, we use `unclass(pryr::obj_size())` instead. 
    
    To predict the size of `x`, we first find out via `object_size(integer(0))` that an integer takes 40 B. For every element of the integer vector additionally 4 B are needed and R allocates memory in chunks of 2, so 8 B at a time. This can be verified for example via `sapply(1:100, function(x) pryr::object_size(integer(x)))`. Overall our prediction will result in 40 B + 1000000 * 4 B = 4000040 B:
    
    
    ```r
    x <- 1:1e6
    unclass(pryr::object_size(x))
    #> [1] 4000048
    ```
    
    To predict the size of `y <- list(x, x)` we make usage of the fact that both list elements point to the same memory and hence are the same reference which means neither one needs additional memory. A list takes 40 B in memory and 8 B for each element (we can verify this in the same way as for integers). Overall our prediction will result in x (4000040 B) + list of length 2 (40 B + 16 B):
    
    
    ```r
    y <- list(x, x)
    unclass(pryr::object_size(y))
    #> [1] 4000112
    ```
    
    Since `x` and `y` are names with bindings to objects that point to the same reference, no additional memory is needed and our prediction is the maximum memory of both objects (y; 4000040 B):
    
    
    ```r
    unclass(pryr::object_size(x, y))
    #> [1] 4000112
    ```
    
    The next one gets a bit more tricky. Since the first element of `y` becomes different to `x`, a completely new object is created in memory. Hence 10 is of type double (which triggers a silent coercion), the new object will take more memory. A double needs 40 B + length * 8 B (overall 8000040 B). So we get: first element of `y` (8000040 B) + second element of `y` (`x`; 4000040 B) + list of length 2 (40 B + 16 B) = 12000136 B as our prediction:
    
    
    ```r
    y[[1]][[1]] <- 10
    unclass(pryr::object_size(y))
    #> [1] 12000160
    ```
    
    Again all elements of `x` are shared within `y` (`x` is the second element of `y`). So the overall memory usage corresponds to `y`'s:
    
    
    ```r
    unclass(pryr::object_size(x, y))
    #> [1] 12000160
    ```
    
    In the next example also the second element of `y` gets the same value as the first one. However, R does not now, that it is the same as the first element, so a new object is created taking the same amount of memory:
    
    
    ```r
    y[[2]][[1]] <- 10
    unclass(pryr::object_size(y))
    #> [1] 16000160
    ```
    
    Now `x` and `y` don't share any values anymore (from Rs perspective) and their memory adds up:
    
    
    ```r
    unclass(pryr::object_size(x, y))
    #> [1] 20000208
    ```

## Modify-in-place

1.  __<span style="color:red">Q</span>__: Wrap the two methods for subtracting medians into two functions, then use the microbenchmark to carefully compare their speeds. How does performance change as the number of columns increase?
    
    __<span style="color:green">A</span>__: We can write one function that handles data frames and lists:
    
    
    ```r
    subtract_medians <- function(x, med){
      for (i in seq_along(medians)) {
        x[[i]] <- x[[i]] - medians[[i]]
      }
      x
    }
    ```
    
    The grid we choose is relatively dense to highlight the progress in the plot below:
    
    
    ```r
    n_grid <- c(seq(from = 1   , to = 9   , by = 1),
                seq(from = 10  , to = 90  , by = 10),
                seq(from = 100 , to = 900 , by = 100),
                seq(from = 1000, to = 5000, by = 1000), 
                10000L)
    ```

    Let's do our benchmark:
    






