

#   __________________ #< 18e203f135e6c85c41aaa4f048f94f75 ># __________________
#   Generate expectations from selection                                    ####


#' @title Generate testhat expectations from selection
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Based on the selection (string of code), a set of testthat \code{expect_*}
#'  statements are generated.
#'
#'  Example: If the selected code is the name of a data frame object,
#'  it will create an \code{\link[testthat:expect_equal]{expect_equal}}
#'  test for each column, along with a test of the column names.
#'
#'  Currently supports side effects (error, warnings, messages),
#'  data frames, vectors, and factors.
#'
#'  List columns in data frames (like nested tibbles) are currently skipped.
#'
#'  Addin: \code{\link[xpectr:insertExpectationsAddin]{insertExpectationsAddin()}}
#' @param selection String of code. (Character)
#'
#'  E.g. \code{"stop('This gives an expect_error test')"}.
#' @param indentation Indentation of the selection. (Numeric)
#' @param tolerance The tolerance for numeric tests as a string, like \code{"1e-4"}. (Character)
#' @param round_to_tolerance Whether to round numeric elements to the specified tolerance. (Logical)
#'
#'  This is currently applied to numeric columns and vectors.
#' @param sample_n The number of elements/rows to sample. Set to \code{NULL} to avoid sampling.
#'
#'  Inserts \code{\link[xpectr:strip]{smpl()}} in the generated tests when sampling was used. A seed is
#'  set internally, setting \code{sample.kind} as \code{"Rounding"} to ensure compatibility with R versions
#'  \code{< 3.6.0}.
#'
#'  The order of the elements/rows is kept intact. No replacement is used, why no oversampling will
#'  take place.
#'
#'  When testing a big data frame, sampling the rows can help keep the test files somewhat readable.
#' @param strip Whether to insert
#'  \code{\link[xpectr:strip]{strip_msg()}} and
#'  \code{\link[xpectr:strip]{strip()}}
#'  in tests of side effects. (Logical)
#'
#'  Sometimes testthat tests have differences in punctuation and newlines on different
#'  systems. By stripping both the error message and the expected message of non-alphanumeric symbols,
#'  we can avoid such failed tests.
#' @param add_wrapper_comments Whether to add intro and outro comments. (Logical)
#' @param add_test_comments Whether to add comments for each test. (Logical)
#' @param assign_output Whether to assign the output of a function call or long selection
#'  to a variable. This will avoid recalling the function and decrease cluttering. (Logical)
#'
#'  Heuristic: when the \code{selection} isn't of a string and contains a paranthesis, it is considered a function call.
#'  A selection with more than 30 characters will be assigned as well.
#'
#'  The tests themselves can be more difficult to interpret, as you will
#'  have to look at the assignment to see the object that is being tested.
#' @param envir Environment to evaluate in.
#' @param out Either \code{"insert"} or \code{"return"}.
#'
#'  \subsection{"insert" (Default)}{
#'  Inserts the expectations via
#'  \code{\link[rstudioapi:insertText]{rstudioapi::insertText()}}.
#'  }
#'  \subsection{"return"}{
#'  Returns the expectations in a list.
#'
#'  These can be prepared for insertion with
#'  \code{\link[xpectr:prepare_insertion]{prepare_insertion()}}.
#'  }
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family expectation generators
#' @export
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' \donttest{
#' df <- data.frame('a' = c(1, 2, 3), 'b' = c('t', 'y', 'u'),
#'                  stringsAsFactors = FALSE)
#'
#' gxs_selection("stop('This gives an expect_error test!')")
#' gxs_selection("warning('This gives an expect_warning test!')")
#' gxs_selection("message('This gives an expect_message test!')")
#' gxs_selection("stop('This: tests the -> punctuation!')", strip = FALSE)
#' gxs_selection("sum(1, 2, 3, 4)")
#' gxs_selection("df")
#'
#' tests <- gxs_selection("df", out = "return")
#' for_insertion <- prepare_insertion(tests)
#' rstudioapi::insertText(for_insertion)
#' }
gxs_selection <- function(selection,
                          indentation = 0,
                          tolerance = "1e-4",
                          round_to_tolerance = TRUE,
                          strip = TRUE,
                          sample_n = 30,
                          envir = NULL,
                          add_wrapper_comments = TRUE,
                          add_test_comments = TRUE,
                          assign_output = TRUE,
                          out = "insert"){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = selection, add = assert_collection)
  checkmate::assert_string(x = tolerance, add = assert_collection)
  checkmate::assert_choice(x = out, choices = c("insert", "return"), add = assert_collection)
  checkmate::assert_flag(x = strip, add = assert_collection)
  checkmate::assert_flag(x = add_wrapper_comments, add = assert_collection)
  checkmate::assert_flag(x = add_test_comments, add = assert_collection)
  checkmate::assert_flag(x = assign_output, add = assert_collection)
  checkmate::assert_flag(x = round_to_tolerance, add = assert_collection)
  checkmate::assert_count(x = indentation, add = assert_collection)
  checkmate::assert_count(x = sample_n, null.ok = TRUE, add = assert_collection)
  checkmate::assert_environment(x = envir, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Set environment if not specified
  if (is.null(envir)) envir <- parent.frame()

  # Check for side effects
  side_effects <- capture_parse_eval_side_effects(selection, envir)
  has_side_effects <- side_effects[["has_side_effects"]]

  if (isTRUE(has_side_effects)) {

    # Create expectations for error, warnings, and messages
    expectations <- create_expectations_side_effect(
      side_effects, name = selection,
      indentation = indentation,
      strip = strip,
      add_wrapper_comments = add_wrapper_comments,
      add_test_comments = add_test_comments)

  } else {

    # Parse and evaluate the selection
    obj <- tryCatch(
      eval_string(selection, envir = envir),
      error = function(e) {
        stop(paste0("Could not parse and evaluate the selection. Threw error:",
                    e))
      },
      warning = function(w) {
        warning(paste0(
          "Got the following warning while parsing and evaluating the selection: ",
          w
        ))
      }
    )

    # TODO perhaps assign_once and evaluate_once aren't the most descriptive var names?
    # If the selection is a very long string
    # we might prefer to assign it once
    assign_once <- nchar(selection) > 30 ||
      (grepl("[\\(\\)]", selection) &&
         !checkmate::test_string(x = obj))

    # If selection is a function call
    if (isTRUE(assign_output) && assign_once){
      evaluate_once <- TRUE
    } else {
      evaluate_once <- FALSE
    }

    # Create expectations based on the type of the objects
    if (is.null(obj)) {
      expectations <- create_expectations_null(name = selection,
                                               indentation = indentation,
                                               add_wrapper_comments = add_wrapper_comments,
                                               add_test_comments = add_test_comments)
    } else if (is.data.frame(obj)) {
      expectations <- create_expectations_data_frame(obj, name = selection,
                                                     indentation = indentation,
                                                     tolerance = tolerance,
                                                     round_to_tolerance = round_to_tolerance,
                                                     sample_n = sample_n,
                                                     add_wrapper_comments = add_wrapper_comments,
                                                     add_test_comments = add_test_comments,
                                                     evaluate_once = evaluate_once)
    } else if (is.vector(obj)) {
      expectations <- create_expectations_vector(obj, name = selection,
                                                 indentation = indentation,
                                                 tolerance = tolerance,
                                                 round_to_tolerance = round_to_tolerance,
                                                 sample_n = sample_n,
                                                 add_wrapper_comments = add_wrapper_comments,
                                                 add_test_comments = add_test_comments,
                                                 evaluate_once = evaluate_once)
    } else if (is.factor(obj)) {
      expectations <- create_expectations_factor(obj, name = selection,
                                                 indentation = indentation,
                                                 tolerance = tolerance,
                                                 sample_n = sample_n,
                                                 add_wrapper_comments = add_wrapper_comments,
                                                 add_test_comments = add_test_comments,
                                                 evaluate_once = evaluate_once)
    } else if (is.function(obj)) {
      expectations <- create_expectations_function(obj, name = selection,
                                                   indentation = indentation,
                                                   sample_n = sample_n,
                                                   add_wrapper_comments = add_wrapper_comments,
                                                   add_test_comments = add_test_comments)
    } else {
      stop(paste0("The selection is not of a currently supported class: ",
                  class(obj)))
    }
  }

  if (out == "insert")
    insert_code(expectations, prepare = TRUE, indentation = indentation)
  else {
    return(expectations)
  }


}
