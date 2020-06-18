

#   __________________ #< 18e203f135e6c85c41aaa4f048f94f75 ># __________________
#   Generate expectations from selection                                    ####


#' @title Generate testhat expectations from selection
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Based on the selection (string of code), a set of \code{testthat} \code{expect_*}
#'  statements are generated.
#'
#'  Example: If the selected code is the name of a \code{data.frame} object,
#'  it will create an \code{\link[testthat:equality-expectations]{expect_equal}}
#'  test for each column, along with a test of the column names,
#'  types and classes, dimensions, grouping keys, etc.
#'
#'  See supported objects in \code{details}.
#'
#'  When testing a function that alters non-local variables, consider enabling \code{`copy_env`}.
#'
#'  Feel free to suggest useful tests etc. in a GitHub issue!
#'
#'  Addin: \code{\link[xpectr:insertExpectationsAddin]{insertExpectationsAddin()}}
#' @param selection String of code. (Character)
#'
#'  E.g. \code{"stop('This gives an expect_error test')"}.
#' @param indentation Indentation of the selection. (Numeric)
#' @param tolerance The tolerance for numeric tests as a string, like \code{"1e-4"}. (Character)
#' @param round_to_tolerance Whether to round numeric elements to the specified tolerance. (Logical)
#'
#'  This is currently applied to numeric columns and vectors (excluding some lists).
#' @param sample_n The number of elements/rows to sample. Set to \code{NULL} to avoid sampling.
#'
#'  Inserts \code{\link[xpectr:strip]{smpl()}} in the generated tests when sampling was used. A seed is
#'  set internally, setting \code{sample.kind} as \code{"Rounding"} to ensure compatibility with \code{R} versions
#'  \code{< 3.6.0}.
#'
#'  The order of the elements/rows is kept intact. No replacement is used, why no oversampling will
#'  take place.
#'
#'  When testing a big \code{data.frame}, sampling the rows can help keep the test files somewhat readable.
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
#'  Heuristic: when the \code{`selection`} isn't of a string and contains a parenthesis, it is considered a function call.
#'  A selection with more than 30 characters will be assigned as well.
#'
#'  The tests themselves can be more difficult to interpret, as you will
#'  have to look at the assignment to see the object that is being tested.
#' @param seed \code{seed} to set. (Whole number)
#' @param test_id Number to append to assignment names. (Whole number)
#'
#'  For instance used to create the \code{"output_"} name: \code{output_<test_id>}.
#' @inheritParams capture_side_effects
#' @param copy_env Whether to work in a deep copy of the environment. (Logical)
#'
#'  Side effects will be captured in copies of the copy, why two copies of the environment will
#'  exist at the same time.
#'
#'  Disabled by default to save memory but is often preferable to enable,
#'  e.g. when the function changes non-local variables.
#' @param out Either \code{"insert"} or \code{"return"}.
#'
#'  \subsection{"insert" (Default)}{
#'  Inserts the expectations via
#'  \code{\link[rstudioapi:rstudio-documents]{rstudioapi::insertText()}}.
#'  }
#'  \subsection{"return"}{
#'  Returns the expectations in a \code{list}.
#'
#'  These can be prepared for insertion with
#'  \code{\link[xpectr:prepare_insertion]{prepare_insertion()}}.
#'  }
#' @param start_with_newline,end_with_newline Whether to have a newline in the beginning/end. (Logical)
#' @return Either \code{NULL} or the unprepared expectations as a \code{character vector}.
#' @details
#'  The following "types" are currently supported or intended to be supported in the future.
#'  Please suggest more types and tests in a GitHub issue!
#'
#'  Note: A set of fallback tests will be generated for unsupported objects.
#'
#'  \tabular{rrr}{
#'   \strong{Type} \tab \strong{Supported} \tab \strong{Notes} \cr
#'   Side effects \tab Yes \tab Errors, warnings, and messages. \cr
#'   Vector \tab Yes \tab Lists are treated differently, depending on their structure. \cr
#'   Factor \tab Yes \tab \cr
#'   Data Frame \tab Yes \tab List columns (like nested tibbles) are currently skipped. \cr
#'   Matrix \tab Yes \tab Supported but could be improved. \cr
#'   Formula \tab Yes \tab \cr
#'   Function \tab Yes \tab \cr
#'   \code{NULL} \tab Yes \tab \cr
#'   Array \tab No \tab \cr
#'   Dates \tab No \tab Base and \code{lubridate}. \cr
#'   ggplot2 \tab No \tab This may be a challenge, but would be cool!\cr
#'  }
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family expectation generators
#' @export
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' \dontrun{
#' df <- data.frame('a' = c(1, 2, 3), 'b' = c('t', 'y', 'u'),
#'                  stringsAsFactors = FALSE)
#'
#' gxs_selection("stop('This gives an expect_error test!')")
#' gxs_selection("warning('This gives a set of side effect tests!')")
#' gxs_selection("message('This also gives a set of side effect tests!')")
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
                          copy_env = FALSE,
                          assign_output = TRUE,
                          seed = 42,
                          test_id = NULL,
                          add_wrapper_comments = TRUE,
                          add_test_comments = TRUE,
                          start_with_newline = TRUE,
                          end_with_newline = TRUE,
                          out = "insert"){

  # Save random seed state
  if (exists(x = ".Random.seed"))
    initial_seed_state <- .Random.seed

  if (is.null(test_id))
    test_id <- floor(runif(1, 10000, 20000))

  # Note: Don't want to use checkmate here
  if (!is.null(seed) && is.numeric(seed) && length(seed) == 1){
    # Set initial seed
    set_test_seed(seed)
    # Save random seed state
    initial_seed_state <- .Random.seed
  } else {
    assign_random_state(initial_seed_state, check_existence = TRUE)
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = selection, add = assert_collection)
  checkmate::assert_string(x = tolerance, add = assert_collection)
  checkmate::assert_choice(x = out, choices = c("insert", "return"), add = assert_collection)
  checkmate::assert_flag(x = strip, add = assert_collection)
  checkmate::assert_flag(x = add_wrapper_comments, add = assert_collection)
  checkmate::assert_flag(x = add_test_comments, add = assert_collection)
  checkmate::assert_flag(x = start_with_newline, add = assert_collection)
  checkmate::assert_flag(x = end_with_newline, add = assert_collection)
  checkmate::assert_flag(x = assign_output, add = assert_collection)
  checkmate::assert_flag(x = round_to_tolerance, add = assert_collection)
  checkmate::assert_flag(x = copy_env, add = assert_collection)
  checkmate::assert_count(x = indentation, add = assert_collection)
  checkmate::assert_count(x = sample_n, null.ok = TRUE, add = assert_collection)
  checkmate::assert_count(x = seed, null.ok = TRUE, add = assert_collection)
  checkmate::assert_count(x = test_id, add = assert_collection)
  checkmate::assert_environment(x = envir, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_number(x = as.numeric(tolerance), upper = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Set environment if not specified
  if (is.null(envir)) envir <- parent.frame()

  # Clone environment if specified
  envir <- clone_env_if(envir = envir, cond = copy_env, deep = TRUE)

  # Check for side effects
  side_effects <- capture_parse_eval_side_effects(string = selection, envir = envir, copy_env = copy_env)
  has_side_effects <- side_effects[["has_side_effects"]]
  has_error <- !is.null(side_effects[["error"]])

  intro_comment <- create_test_comment(
    selection,
    section = "intro",
    indentation = indentation,
    create_comment = add_wrapper_comments
  )
  outro_comment <-  create_test_comment(
    selection,
    section = "outro",
    indentation = indentation,
    create_comment = add_wrapper_comments
  )
  seed_setting_string <- paste0("xpectr::set_test_seed(", seed, ")")

  expectations <- c()
  sfx_expectations <- c()

  if (isTRUE(has_side_effects)) {

    # Create expectations for error, warnings, and messages
    sfx_expectations <- create_expectations_side_effect(
      side_effects, name = selection,
      copy_env = copy_env,
      indentation = indentation,
      add_comments = add_test_comments,
      strip = strip,
      test_id = test_id)

    selection <- paste0("xpectr::suppress_mw(", selection, ")")

  }

  if (!isTRUE(has_error)) {

    # Parse and evaluate the selection
    obj <- tryCatch({
      # Reset seed
      # Note: Only seems to work when setting it in globalenv
      # but it's the seed we started with, so it shouldn't be a problem?
      assign_random_state(initial_seed_state, check_existence = TRUE)

      # Evaluate string
      eval_string(selection, envir = envir)
    }, error = function(e) {
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

    # Check if object is a side effect string
    # This happens when the warning/message is called last in a function
    if (isTRUE(has_side_effects)){
      obj_is_side_effect_string <- checkmate::test_string(obj) &&
        obj %in% unlist(side_effects[c("error", "warnings", "messages")],
                        recursive = TRUE, use.names = FALSE)
    } else {
      obj_is_side_effect_string <- FALSE
    }

    if (!isTRUE(obj_is_side_effect_string)){

      # TODO perhaps assign_once and evaluate_once aren't the most descriptive var names?
      # If the selection is a very long string
      # we might prefer to assign it once
      assign_once <- nchar(selection) > 30 ||
        (grepl("[\\(\\)]", selection) &&
           !(checkmate::test_string(x = obj) &&
               add_quotation_marks(obj) == selection))

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
                                                 add_comments = add_test_comments)
      } else if (is.data.frame(obj)) {
        expectations <- create_expectations_data_frame(obj, name = selection,
                                                       indentation = indentation,
                                                       tolerance = tolerance,
                                                       round_to_tolerance = round_to_tolerance,
                                                       sample_n = sample_n,
                                                       add_comments = add_test_comments,
                                                       evaluate_once = evaluate_once,
                                                       test_id = test_id)
      } else if (is.matrix(obj)) {
        expectations <- create_expectations_matrix(obj, name = selection,
                                                   indentation = indentation,
                                                   tolerance = tolerance,
                                                   round_to_tolerance = round_to_tolerance,
                                                   sample_n = sample_n,
                                                   add_comments = add_test_comments,
                                                   evaluate_once = evaluate_once,
                                                   test_id = test_id)
      } else if (is.vector(obj)) {
        expectations <- create_expectations_vector(obj, name = selection,
                                                   indentation = indentation,
                                                   tolerance = tolerance,
                                                   round_to_tolerance = round_to_tolerance,
                                                   sample_n = sample_n,
                                                   add_comments = add_test_comments,
                                                   evaluate_once = evaluate_once,
                                                   test_id = test_id)
      } else if (is.factor(obj)) {
        expectations <- create_expectations_factor(obj, name = selection,
                                                   indentation = indentation,
                                                   tolerance = tolerance,
                                                   sample_n = sample_n,
                                                   add_comments = add_test_comments,
                                                   evaluate_once = evaluate_once,
                                                   test_id = test_id)
      } else if (is.function(obj)) {
        expectations <- create_expectations_function(obj, name = selection,
                                                     indentation = indentation,
                                                     sample_n = sample_n,
                                                     add_comments = add_test_comments,
                                                     evaluate_once = evaluate_once,
                                                     test_id = test_id)
      } else if (rlang::is_formula(obj)) {
        expectations <- create_expectations_formula(obj, name = selection,
                                                    indentation = indentation,
                                                    add_comments = add_test_comments,
                                                    evaluate_once = evaluate_once,
                                                    test_id = test_id)
      } else {
        warning(paste0("The selection is not of a currently supported class '",
                       class(obj),"'. Will generate fallback tests."))
        expectations <- create_expectations_fallback(obj, name = selection,
                                                    indentation = indentation,
                                                    add_comments = add_test_comments,
                                                    evaluate_once = evaluate_once,
                                                    test_id = test_id)
      }
    }
  }

  expectations <- c(intro_comment,
                    seed_setting_string,
                    sfx_expectations,
                    expectations,
                    outro_comment)

  # Add newlines before and after test block
  if (isTRUE(start_with_newline))
    expectations <- c(" ", expectations)
  if (isTRUE(end_with_newline))
    expectations <- c(expectations, " ")


  if (out == "insert"){
    insert_code(expectations, prepare = TRUE, indentation = indentation)
  } else {
    return(expectations)
  }


}
