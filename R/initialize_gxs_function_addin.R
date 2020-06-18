

#   __________________ #< 60cfc78f594e5611a6eaaf34a2b212ae ># __________________
#   Initialize gxs_function call                                            ####


#' @title Initialize \code{gxs_function()} call
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Initializes the \code{gxs_function()} call with the arguments and default values
#'  of the selected function.
#'
#'  See \code{`Details`} for how to set a key command.
#' @param selection Name of function to test with \code{gxs_function()}. (Character)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param indentation Indentation of the selection. (Numeric)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @param insert Whether to insert the code via
#'  \code{\link[rstudioapi:rstudio-documents]{rstudioapi::insertText()}}
#'  or return them. (Logical)
#'
#'  \strong{N.B.} Mainly intended for testing the addin programmatically.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @family expectation generators
#' @family addins
#' @export
#' @return Inserts \code{\link[xpectr:gxs_function]{gxs_function()}} call for
#'  the selected function.
#'
#'  Returns \code{NULL} invisibly.
#' @details
#'  \subsection{How}{
#'  Parses and evaluates the selected code string
#'  within the parent environment.
#'  When the output is a function, it extracts the formals (arguments and default values)
#'  and creates the initial \code{`args_values`} for \code{\link[xpectr:gxs_function]{gxs_function()}}.
#'  When the output is not a function, it throws an error.
#'  }
#'  \subsection{How to set up a key command in RStudio}{
#'
#'  After installing the package.
#'  Go to:
#'
#'  \code{Tools >> Addins >> Browse Addins >> Keyboard Shortcuts}.
#'
#'  Find \code{"Initialize gxs_function()"} and press its field under \code{Shortcut}.
#'
#'  Press desired key command, e.g. \code{Alt+F}.
#'
#'  Press \code{Apply}.
#'
#'  Press \code{Execute}.
#'  }
initializeGXSFunctionAddin <- function(selection = NULL, insert = TRUE, indentation = 0) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(x = selection, null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_flag(x = insert, add = assert_collection)
  checkmate::assert_integerish(x = indentation, lower = 0,
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Get the selection and indentation
  if (is.null(selection)){
    selection <- tryCatch(
      get_selection(),
      error = function(e) {
        return("")
      }
    )
    indentation <- tryCatch(
      get_indentation(),
      error = function(e) {
        return(0)
      }
    )
  }

  # Get parent environment
  parent_envir <- parent.frame()

  #  Create expectations

  if (selection != "") {

    # Parse and evaluate the selection
    obj <- tryCatch({
      # Evaluate string
      eval_string(selection, envir = parent_envir)
    }, error = function(e) {
      stop(paste0("Could not parse and evaluate the selection. Threw error:",
                  e))
    },
    warning = function(w) {
      warning(paste0(
        "Got the following warning while parsing and evaluating the selection: ",
        w
      ))
    })

    if (!checkmate::test_function(x = obj)){
      stop("'selection' was not the name of an available function.")
    }

    # Trim trailing spaces
    selection <- trimws(selection, which = "both")

    # Extract the formals (arg names and default values)
    fn_formals <- formals(obj)
    arg_names <- names(fn_formals)

    # Create args_values elements
    args_values_strings <- plyr::llply(arg_names, function(an){
      paste0("\"", an, "\" = list(", paste0(deparse(fn_formals[[an]])), ")")
    }) %>%
      unlist() %>%
      paste0(collapse = ",\n") %>%
      strsplit(split = "\n") %>%
      unlist(recursive = TRUE, use.names = FALSE)

    # Add 4 spaces in front of args_values elements
    spaces <- create_space_string(n = 4)
    args_values_strings <- paste0(spaces, args_values_strings)

    # Create the strings for insertion
    to_insert <- c(
      paste0("# Generate expectations for '", selection, "'"),
      "# Tip: comment out the gxs_function() call",
      "# so it is easy to regenerate the tests",
      "xpectr::set_test_seed(42)",
      "xpectr::gxs_function(",
      paste0("  fn = ", selection, ","),
      "  args_values = list(\n",
      args_values_strings,
      "  ),",
      "  indentation = 2,",
      "  copy_env = FALSE",
      ")",
      " ",
      "#"
    )

    # Insert or return
    if (isTRUE(insert)){
      insert_code(to_insert, prepare = TRUE, indentation = indentation)
    } else {
      return(to_insert)
    }
  }

  invisible()
}
