#' Define the multiverse decision space
#'
#' Manually specifies the set of decision nodes and their branches that
#' define the multiverse of analyses.
#'
#' @param ... `mvrs_decision_node` objects.
#' @return An S3 object of class `mvrs_decisions`.
#' @export
mvrs_decisions <- function(...) {
  nodes <- list(...)
  if (length(nodes) == 0) {
    abort("At least one decision node is required.")
  }
  if (!all(vapply(nodes, inherits, logical(1), "mvrs_decision_node"))) {
    abort("All arguments must be `mvrs_decision_node` objects.")
  }

  structure(nodes, class = "mvrs_decisions")
}

#' Create a single decision node
#'
#' A decision node represents one analyst degree of freedom with multiple
#' possible branches (choices).
#'
#' @param name Character string naming this decision.
#' @param branches Character vector of possible choices.
#' @return An S3 object of class `mvrs_decision_node`.
#' @export
mvrs_decision_node <- function(name, branches) {
  if (!is.character(name) || length(name) != 1) {
    abort("`name` must be a single character string.")
  }
  if (!is.character(branches) || length(branches) < 2) {
    abort("`branches` must be a character vector with at least 2 options.")
  }

  structure(
    list(name = name, branches = branches),
    class = "mvrs_decision_node"
  )
}

#' Discover decision space using LLM analyst agents
#'
#' Uses LLM analyst agents to identify analyst degrees of freedom given
#' a dataset and research question. Requires connection to the multivrs
#' CLI orchestrator or API.
#'
#' @param dataset A `mvrs_dataset` object.
#' @param question A `mvrs_question` object.
#' @param provider Optional provider configuration for the LLM backend.
#' @return A `mvrs_decisions` object (currently a stub).
#' @export
mvrs_discover <- function(dataset, question, provider = NULL) {
  if (!inherits(dataset, "mvrs_dataset")) {
    abort("`dataset` must be a `mvrs_dataset` object.")
  }
  if (!inherits(question, "mvrs_question")) {
    abort("`question` must be a `mvrs_question` object.")
  }

  warn(c(
    "LLM-assisted discovery is not yet implemented.",
    "i" = "Define decisions manually with `mvrs_decisions()` and `mvrs_decision_node()`.",
    "i" = "Full integration with the multivrs CLI orchestrator is coming soon."
  ))

  invisible(NULL)
}

#' @export
print.mvrs_decisions <- function(x, ...) {
  n_nodes <- length(x)
  branch_counts <- vapply(x, function(node) length(node$branches), integer(1))
  total_specs <- prod(branch_counts)

  cli_alert_info("Decision space: {n_nodes} node(s)")
  for (node in x) {
    cli_alert_info(
      "  {.val {node$name}}: {length(node$branches)} branches ({paste(node$branches, collapse = ', ')})"
    )
  }
  cli_alert_success("Total specifications: {.val {total_specs}}")
  invisible(x)
}
