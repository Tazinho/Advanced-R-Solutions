# Specific for this project -------------------------------------
Sys.setenv(LANGUAGE = "en")

# Load a few common packages
`%>%` <- magrittr::`%>%`

# Experiment with options output
# options(width = 50)


# Increase Font Size of plots
ggplot2::theme_set(
  ggplot2::theme_gray(base_size = 15)
)

# Inherited from Advanced R -------------------------------------
library(methods)
set.seed(1014)

# options(lifecycle_warnings_as_errors = TRUE)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  cache = TRUE,
  fig.retina = 0.8, # figures are either vectors or 300 dpi diagrams
  dpi = 300,
  out.width = "70%",
  fig.align = "center",
  fig.width = 6,
  fig.asp = 0.618, # 1 / phi
  fig.show = "hold"
)

options(
  rlang_trace_top_env = rlang::current_env(),
  rlang_backtrace_on_error = "none"
)

options(
  digits = 3,
  str = strOptions(strict.width = "cut")
)

if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(width = 69)
  options(width = 69)
  options(crayon.enabled = FALSE)
  options(cli.unicode = TRUE)
}

knitr::knit_hooks$set(
  small_mar = function(before, options, envir) {
    if (before) {
      par(mar = c(4.1, 4.1, 0.5, 0.5))
    }
  }
)

error_wrap <- function(x, width = getOption("width")) {
  lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
  paste(strwrap(lines, width = width), collapse = "\n")
}
