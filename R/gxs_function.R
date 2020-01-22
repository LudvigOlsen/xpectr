

#   __________________ #< 843da5fbc35d01231a00f0237a49eb51 ># __________________
#   Generate function call tests                                            ####


# First arg value should be valid!

#' @title Generate testhat expectations for argument values in a function
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Based on a set of supplied values for each function argument,
#'  a set of testthat \code{expect_*} statements are generated.
#'
#'  \strong{Included tests}: The first value supplied for an argument
#'  is considered the \emph{valid baseline} value. For each argument, we
#'  create tests for each of the supplied values, where the other arguments
#'  have their baseline value.
#'
#'  Currently, supported tests are of side effects (error, warnings, messages),
#'  data frames, vectors, and factors. List columns in data frames (like nested tibbles) are skipped.
#'
#'  \strong{N.B.} This function is undergoing active development!
#' @param fn Function to create tests for.
#' @param args_values The arguments and the values to create tests for.
#'  Should be supplied as a named list of lists, like the following:
#'
#'  \code{args_values = list("x1" = list(1,2,3), "x2" = list("a","b","c"))}
#'
#'  The first value for each argument (referred to as the 'baseline' value) should be valid
#'  (not throw an error/message/warning).
#'  \strong{N.B.} This is not checked but should lead to more meaningful tests.
#'
#'  \strong{N.B.} Please define the list directly in the function call.
#'  This is currently necessary.
#' @param check_nulls Whether to try all arguments with \code{NULL}. (Logical)
#'
#'  Note: With this enabled, you don't need to add \code{NULL} to your \code{args_values},
#'  unless it should be the baseline value.
#' @inheritParams gxs_selection
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family expectation generators
#' @export
#' @examples
#' # Attach packages
#' library(xpectr)
#'
#' \donttest{
#' fn <- function(x, y, z){
#'   if (x>3) stop("'x' > 3")
#'   if (y<0) warning("'y'<0")
#'   if (z==10) message("'z' was 10!")
#'   x + y + z
#' }
#'
#' # Create expectations
#' # Note: define the list in the call
#' gxs_function(fn,
#'              list("x" = list(2, 4, NA),
#'                   "y" = list(0, -1),
#'                   "z" = list(5, 10, 15)))
#' }
gxs_function <- function(fn,
                         args_values,
                         check_nulls = TRUE,
                         indentation = 0,
                         strip = TRUE,
                         tolerance = "1e-4",
                         envir = NULL,
                         sample_n = 30,
                         add_wrapper_comments = TRUE,
                         add_test_comments = TRUE,
                         out = "insert"){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_function(x = fn, add = assert_collection)
  checkmate::assert_list(x = args_values, types = c("list"),
                         names = "named", add = assert_collection)
  checkmate::assert_flag(x = check_nulls, add = assert_collection)
  checkmate::assert_string(x = tolerance, add = assert_collection)
  checkmate::assert_choice(x = out, choices = c("insert", "return"), add = assert_collection)
  checkmate::assert_flag(x = strip, add = assert_collection)
  checkmate::assert_flag(x = add_wrapper_comments, add = assert_collection)
  checkmate::assert_flag(x = add_test_comments, add = assert_collection)
  checkmate::assert_count(x = indentation, add = assert_collection)
  checkmate::assert_count(x = sample_n, null.ok = TRUE, add = assert_collection)
  checkmate::assert_environment(x = envir, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(x = names(args_values), what = "argnames",
                          type = "unique", add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Get deparsed fn name
  fn_name <- deparse(substitute(fn))

  # Understanding arg_call:
  #   The substituted list contains each sublist
  #   Each element in those are 1: list/c, 2: first element, 3: second element, etc.
  arg_call <- substitute(args_values)

  if (!grepl("[\\(\\)]", collapse_strings(deparse(arg_call)), fixed = FALSE)){
    assert_collection$push("Please define the 'arg_values' list directly in the function call.")
    checkmate::reportAssertions(assert_collection)
  }

  # Generate function call fn(arg = value) strings
  fn_call_strings <- generate_function_strings(fn_name = fn_name,
                                               args_values_substituted = arg_call,
                                               check_nulls = check_nulls)

  if (is.null(envir)) envir <- parent.frame()

  expectations <- plyr::llply(fn_call_strings, function(string){
    c(create_test_comment(string, create_comment = add_test_comments),
    gxs_selection(
      selection = string,
      indentation = indentation,
      strip = strip,
      tolerance = tolerance,
      envir = envir,
      sample_n = sample_n,
      add_test_comments = add_test_comments,
      add_wrapper_comments = FALSE,
      out = "return"
    ), "")
  }) %>% unlist(recursive = TRUE)

  # Add comments
  expectations <- c(create_test_comment(fn_name, section = "intro",
                                        create_comment = add_wrapper_comments),
                    create_test_comment("different combinations of argument values",
                                        create_comment = add_test_comments),
                    "",
                    expectations,
                    create_test_comment(fn_name, section = "outro",
                                        create_comment = add_wrapper_comments))

  if (out == "insert")
    insert_code(expectations, prepare = TRUE, indentation = indentation)
  else
    return(expectations)

}


#   __________________ #< 6b3bf3f6dd0e841216f9dd02e6b4ba8f ># __________________
#   Generate function call strings                                          ####


generate_function_strings <- function(fn_name,
                                      args_values_substituted,
                                      check_nulls = TRUE){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = fn_name, add = assert_collection)
  checkmate::assert_flag(x = check_nulls, add = assert_collection)
  if (grepl("[\\(\\)]", fn_name, fixed = FALSE)){
    assert_collection$push("'fn_name' cannot contain parantheses. Must be a function name.")
  }
  if (!is.language(args_values_substituted)){
    assert_collection$push(
      paste0("'args_values_substituted' must be a language object. It shou",
             "ld be made with substitute(args_values), where 'args_values' i",
             "s a named list of lists."))
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Get argument names
  arg_names <- non_empty_names(args_values_substituted)

  # Create a tibble of the substituted values
  tibbled_args_values <- plyr::ldply(arg_names, function(an){
    plyr::llply(args_values_substituted[[an]], function(av){
      deparse(av)
    }) %>% tibble::enframe(name = "index") %>%
      dplyr::mutate(arg_name = an,
                    index = .data$index - 1)
  }) %>% dplyr::filter(.data$index != 0)

  default_values <- tibbled_args_values %>%
    dplyr::filter(.data$index == 1)

  non_default_values <- tibbled_args_values %>%
    dplyr::filter(.data$index != 1)

  # Generate the combinations of argument values
  combinations <- plyr::ldply(seq_len(nrow(non_default_values)), function(r){
    current_row <- non_default_values[r,]
    current_arg <- current_row[1, "arg_name"]
    dplyr::bind_rows(
      default_values %>% dplyr::filter(.data$arg_name != current_arg),
      current_row
    ) %>%
      dplyr::mutate(combination = as.character(r))
  }) %>%
    dplyr::bind_rows(default_values %>% dplyr::mutate(combination = "default"))

  # Create NULL checks
  if (isTRUE(check_nulls)){
    null_combinations <- plyr::ldply(arg_names, function(an){
      d <- default_values
      if (d[d[["arg_name"]] == an, "value"] == "NULL"){
        return(NULL)
      }
      d[d[["arg_name"]] == an, "value"] <- "NULL" # TODO check if this can go wrong?
      d %>% dplyr::mutate(combination = an)
    })

    combinations <- dplyr::bind_rows(
      combinations, null_combinations
    )
  }

  # Sort by argument name order
  # So we call in same order as in args_values
  combinations <- combinations %>%
    dplyr::right_join(
      tibble::enframe(arg_names, name = NULL, value = "arg_name"),
      by = "arg_name"
    )

  function_call_strings <- combinations %>%
    dplyr::mutate(name_value = paste0(.data$arg_name," = ", .data$value)) %>%
    dplyr::group_by(.data$combination) %>%
    dplyr::summarise(call_strings = paste0(
      fn_name,"(", paste0(.data$name_value, collapse = ", "), ")")) %>%
    dplyr::pull(.data$call_strings) %>%
    unique()

  function_call_strings
}
