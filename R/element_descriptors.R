

#   __________________ #< 60cfc78f594e5611a6eaaf34a2b212ae ># __________________
#   Element descriptors                                                     ####


##  .................. #< 440b147b963f8a7fd202661bfc3b068e ># ..................
##  Element lengths                                                         ####


#' @title Gets the length of each element
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Applies \code{\link[base:length]{length()}} to each element of \code{x} (without recursion).
#' @param x List with elements.
#' @param keep_names Whether to keep names. (Logical)
#' @family element descriptors
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
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
  unlist(lapply(x, length), use.names = keep_names)
}


##  .................. #< abd5e8f22decefad01cca729a155076c ># ..................
##  Element types                                                           ####


#' @title Gets the type of each element
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Applies \code{\link[base:typeof]{typeof()}} to each element of \code{x} (without recursion).
#' @inheritParams element_lengths
#' @family element descriptors
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
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
  unlist(lapply(x, typeof), use.names = keep_names)
}


##  .................. #< cad874f32d32a8eae09090d2894d7ad5 ># ..................
##  Element classes                                                         ####


#' @title Gets the class of each element
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Applies \code{\link[base:class]{class()}} to each element of \code{x} (without recursion).
#' @inheritParams element_lengths
#' @family element descriptors
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @export
#' @details
#'  Simple wrapper for \code{unlist(lapply(x, class))}.
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' l <- list("a" = c(1,2,3), "b" = "a", "c" = NULL)
#'
#' element_classes(l)
#' element_classes(l, keep_names = TRUE)
element_classes <- function(x, keep_names = FALSE){
  unlist(lapply(x, class), use.names = keep_names)
}

