

#   __________________ #< abd5e8f22decefad01cca729a155076c ># __________________
#   Sample                                                                  ####


#' @title Random sampling
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Sample a vector, factor or data frame. Useful to reduce size of testthat \code{expect_*} tests.
#'  Not intended for other purposes.
#'
#'  Wraps \code{\link[base:sample.int]{sample.int()}}. Data frames are sampled row-wise.
#'
#'  The seed is set within the function with \code{sample.kind} as \code{"Rounding"}
#'  for compatibility with R versions \code{< 3.6.0}. On exit, the random state is restored.
#' @param data Vector or data frame. (Logical)
#' @param n Number of elements/rows to sample.
#'
#'  \strong{N.B.} No replacement is used, why \code{n >} the number of elements/rows in \code{data} won't
#'  perform oversampling.
#' @param keep_order Whether to keep the order of the elements. (Logical)
#' @param seed Seed to use.
#'
#'  The seed is set with \code{sample.kind = "Rounding"}
#'  for compatibility with R versions \code{< 3.6.0}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @return When \code{data} has \code{<=n} elements, \code{data} is returned.
#'  Otherwise, \code{data} is sampled and returned.
#' @export
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' smpl(c(1,2,3,4,5), n = 3)
#' smpl(data.frame("a" = c(1,2,3,4,5), "b" = c(2,3,4,5,6), stringsAsFactors = FALSE), n = 3)
smpl <- function(data,
                 n,
                 keep_order = TRUE,
                 seed = 42) {

  if (exists(".Random.seed"))
    initial_random_state <- .Random.seed

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_count(x = n,
                          positive = TRUE,
                          add = assert_collection)
  checkmate::assert_count(x = seed,
                          positive = TRUE,
                          add = assert_collection)
  checkmate::assert_flag(x = keep_order, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Set seed to be compatible with R before and after v3.6
  set_test_seed(seed)

  # Sample data
  if (is.data.frame(data)) {
    if (nrow(data) <= n) {
      if (isTRUE(keep_order))
        return(data)
      else
        n <- nrow(data)
    }

    indices <- sample.int(n = nrow(data),
                          size = n,
                          replace = FALSE)

    if (isTRUE(keep_order))
      indices <- sort(indices)

    data <- data[indices,]

  } else if (is.vector(data) || is.factor(data)) {
    if (length(data) <= n) {
      if (isTRUE(keep_order))
        return(data)
      else
        n <- length(data)
    }

    indices <- sample.int(n = length(data),
                          size = n,
                          replace = FALSE)

    if (isTRUE(keep_order))
      indices <- sort(indices)

    data <- data[indices]

  } else {
    stop("Only vectors, factors and data frames are currently supported.")
  }

  # Reassign the previous random state
  assign_random_state(initial_random_state, check_existence = TRUE)

  data

}
