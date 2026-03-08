#' Enumerate all specification combinations
#'
#' Takes a decision space and expands it into a tibble where each row
#' is one unique specification (one combination of branches across all
#' decision nodes).
#'
#' @param decisions A `mvrs_decisions` object.
#' @return A tibble with one column per decision node and one row per
#'   specification.
#' @export
mvrs_enumerate_specs <- function(decisions) {
  if (!inherits(decisions, "mvrs_decisions")) {
    abort("`decisions` must be a `mvrs_decisions` object.")
  }

  branch_lists <- stats::setNames(
    lapply(decisions, function(node) node$branches),
    vapply(decisions, function(node) node$name, character(1))
  )

  specs <- expand.grid(branch_lists, stringsAsFactors = FALSE)
  tibble::as_tibble(specs)
}
