

#   __________________ #< 60cfc78f594e5611a6eaaf34a2b212ae ># __________________
#   Element descriptors                                                     ####


##  .................. #< 440b147b963f8a7fd202661bfc3b068e ># ..................
##  Element lengths                                                         ####


#' @title Gets the length of each element
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Applies \code{\link[base:length]{length()}} to each element of \code{`x`} (without recursion).
#' @param x List with elements.
#' @param keep_names Whether to keep existing names. (Logical)
#' @family element descriptors
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return The length of each element.
#' @export
#' @details
#'  Simple wrapper for \code{unlist(lapply(x, length))}.
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' l <- list("a" = c(1,2,3), "b" = 1, "c" = NULL)
#'
#' element_lengths(l)
#' element_lengths(l, keep_names = TRUE)
element_lengths <- function(x, keep_names = FALSE){
  checkmate::assert_flag(
    x = keep_names,
    na.ok = FALSE,
    null.ok = FALSE,
  )
  unlist(lapply(x, length), use.names = keep_names)
}


##  .................. #< abd5e8f22decefad01cca729a155076c ># ..................
##  Element types                                                           ####


#' @title Gets the type of each element
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Applies \code{\link[base:typeof]{typeof()}} to each element of \code{`x`} (without recursion).
#' @inheritParams element_lengths
#' @family element descriptors
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return The type of each element.
#' @details
#'  Simple wrapper for \code{unlist(lapply(x, typeof))}.
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' l <- list("a" = c(1,2,3), "b" = "a", "c" = NULL)
#'
#' element_types(l)
#' element_types(l, keep_names = TRUE)
element_types <- function(x, keep_names = FALSE){
  checkmate::assert_flag(
    x = keep_names,
    na.ok = FALSE,
    null.ok = FALSE,
  )
  unlist(lapply(x, typeof), use.names = keep_names)
}


##  .................. #< cad874f32d32a8eae09090d2894d7ad5 ># ..................
##  Element classes                                                         ####


#' @title Gets the class of each element
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Applies \code{\link[base:class]{class()}} to each element of \code{`x`} (without recursion). When
#'  \code{class()} returns multiple strings, the first class string is returned.
#' @inheritParams element_lengths
#' @family element descriptors
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return The main class of each element.
#' @details
#'  Gets first string in \code{class()} for all elements.
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' l <- list("a" = c(1,2,3), "b" = "a", "c" = NULL)
#'
#' element_classes(l)
#' element_classes(l, keep_names = TRUE)
element_classes <- function(x, keep_names = FALSE){
  checkmate::assert_flag(
    x = keep_names,
    na.ok = FALSE,
    null.ok = FALSE,
  )
  first_class <- function(x){class(x)[[1]]}
  unlist(lapply(x, first_class), use.names = keep_names)
}


##  .................. #< 16b58b1186e423259991634cf27c01de ># ..................
##  Total number of elements                                                ####

#' @title Total number of elements
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Unlists \code{`x`} recursively and finds the total number of elements.
#' @param x List with elements.
#' @param deduplicated Whether to only count the unique elements. (Logical)
#' @family element descriptors
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @return The total number of elements in \code{`x`}.
#' @details
#'  Simple wrapper for
#'  \code{length(unlist(x, recursive = TRUE, use.names = FALSE))}.
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' l <- list(list(list(1, 2, 3), list(2, list(3, 2))),
#'           list(1, list(list(2, 4), list(7, 1, list(3, 8)))),
#'           list(list(2, 7, 8), list(10, 2, list(18, 1, 4))))
#'
#' num_total_elements(l)
#' num_total_elements(l, deduplicated = TRUE)
num_total_elements <- function(x, deduplicated = FALSE){
  checkmate::assert_flag(
    x = deduplicated,
    na.ok = FALSE,
    null.ok = FALSE,
  )
  elems <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (isTRUE(deduplicated))
    elems <- unique(elems)
  length(elems)
}


