
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xpectr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/xpectr)](https://CRAN.R-project.org/package=xpectr)
<!-- badges: end -->

`xpectr` provides a set of utilities and RStudio addins for generating
tests for testthat unittesting.

**Author:** [Ludvig R. Olsen](http://ludvigolsen.dk/) (
<r-pkgs@ludvigolsen.dk> ) <br/> **Started:** January 2020

## Installation

You can install the development version with:

``` r
devtools::install_github("ludvigolsen/xpectr")
```

## Main functions

Note: `gxs` stands for `generate expectations`.

  - `gxs_selection()`
  - `strip()`
  - `strip_msg()`
  - `capture_side_effects()`
  - `capture_parse_eval_side_effects()`

## Addins

  - `Insert Expectations` : generates `testthat` `expect_*` tests from
    selected code
  - `dput() selected` : applies `dput()` to selected code
  - `Insert checkmate AssertCollection code` : inserts code for
    initializing and reporting a checkmate AssertCollection
  - `Wrap string with paste0` : splits selected string every n
    characters and wraps in `paste0` call

## Using in packages

It is suggested to add `xpectr` in the `Suggests` field in the
`DESCRIPTION` file. This allows you to use it in unit tests and users
aren’t required to have it installed.

NOTE: This won’t work on CRAN before it is released there.

## Examples

``` r
library(xpectr)
library(testthat)
```

``` r
# Some data

num_vec <- 1:10
char_vec <- head(LETTERS, 10)

df <- data.frame(
  'a' = c(1, 2, 3),
  'b' = c('t', 'y', 'u'),
  stringsAsFactors = FALSE
)

# A function with side effects
fn <- function(raise = FALSE){
  message("Hi! I'm Kevin, your favorite message!")
  warning("G'Day Mam! I'm a warning to the world!")
  message("Kevin is ma name! Yesss!")
  warning("Hopefully the whole world will see me :o")
  if (isTRUE(raise)){
    stop("Lord Evil Error has arrived! Yeehaaa")
  }
}
```

### `gxs_selection()`

#### Selection is a vector

``` r
gxs_selection("num_vec")

# Inserts the following tests:

expect_equal(
  num_vec,
  1:10,
  tolerance = 1e-4)

gxs_selection("char_vec")

# Inserts the following tests:

expect_equal(
  char_vec,
  c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
  fixed = TRUE)
```

#### Selection is a data frame

``` r
gxs_selection("df")

# Inserts the following tests:

expect_equal(
  df[["a"]],
  c(1, 2, 3),
  tolerance = 1e-4)
expect_equal(
  df[["b"]],
  c("t", "y", "u"),
  fixed = TRUE)
expect_equal(
  names( df ),
  c("a", "b"),
  fixed = TRUE)
```

#### Selection is a function with side effects

Note that the `strip_msg` and `strip` functions are wrapped around the
side effect and the message. These remove non-alphanumeric symbols from
the tested strings. When running `testthat` unit tests on different
systems, they sometimes vary in the use of punctuation and newlines
(\\n). These functions will help the `expect_*` function to ignore that.
In case such differences are important to catch, you can set `strip =
FALSE` in `gxs_selection()`.

``` r
gxs_selection("fn()")

# Inserts the following tests:

expect_warning(
  xpectr::strip_msg(fn()),
  xpectr::strip("G'Day Mam! I'm a warning to the world!"),
  fixed = TRUE)
expect_warning(
  xpectr::strip_msg(fn()),
  xpectr::strip("Hopefully the whole world will see me :o"),
  fixed = TRUE)
expect_message(
  xpectr::strip_msg(fn()),
  xpectr::strip("Hi! I'm Kevin, your favorite message!\n"),
  fixed = TRUE)
expect_message(
  xpectr::strip_msg(fn()),
  xpectr::strip("Kevin is ma name! Yesss!\n"),
  fixed = TRUE)

# In case of errors, the warnings and messages aren't tested

gxs_selection("fn(raise = TRUE)")

# Inserts the following tests:

expect_error(
  xpectr::strip_msg(fn(raise = TRUE)),
  xpectr::strip("Lord Evil Error has arrived! Yeehaaa"),
  fixed = TRUE)
```
