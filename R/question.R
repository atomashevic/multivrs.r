#' Define a research question
#'
#' Specifies the model blueprint for a multiverse analysis: the outcome,
#' predictor of interest, optional controls, and expected direction.
#'
#' @param outcome Character string naming the outcome variable.
#' @param predictor Character string naming the predictor of interest.
#' @param controls Optional character vector of control variable names.
#' @param levels Optional named list specifying variable levels or coding.
#' @param direction Optional expected direction: `"positive"`, `"negative"`,
#'   or `NULL` (no expectation).
#' @return An S3 object of class `mvrs_question`.
#' @export
mvrs_question <- function(outcome, predictor, controls = NULL, levels = NULL,
                          direction = NULL) {
  if (!is.character(outcome) || length(outcome) != 1) {
    abort("`outcome` must be a single character string.")
  }
  if (!is.character(predictor) || length(predictor) != 1) {
    abort("`predictor` must be a single character string.")
  }
  if (!is.null(direction)) {
    direction <- match.arg(direction, c("positive", "negative"))
  }

  structure(
    list(
      outcome = outcome,
      predictor = predictor,
      controls = controls,
      levels = levels,
      direction = direction
    ),
    class = "mvrs_question"
  )
}

#' @export
print.mvrs_question <- function(x, ...) {
  cli_alert_info("Research question: {.val {x$outcome}} ~ {.val {x$predictor}}")
  if (!is.null(x$controls)) {
    cli_alert_info("Controls: {paste(x$controls, collapse = ', ')}")
  }
  if (!is.null(x$direction)) {
    cli_alert_info("Expected direction: {.val {x$direction}}")
  }
  invisible(x)
}
