#' Create a multiverse results object
#'
#' @param specs A tibble of specification combinations.
#' @param estimates A tibble of results with columns `estimate`, `std_error`,
#'   `p_value`, and `significant`.
#' @param question The `mvrs_question` used.
#' @param dataset_name Name of the dataset analysed.
#' @return An S3 object of class `mvrs_results`.
#' @keywords internal
mvrs_results <- function(specs, estimates, question, dataset_name) {
  structure(
    list(
      specs = specs,
      estimates = estimates,
      question = question,
      dataset_name = dataset_name
    ),
    class = "mvrs_results"
  )
}

#' Specification curve plot
#'
#' Produces a specification curve visualization: a panel of sorted effect
#' estimates with indicators for which decisions were active in each
#' specification.
#'
#' @param results A `mvrs_results` object.
#' @return A `ggplot` object.
#' @export
mvrs_spec_curve <- function(results) {
  if (!inherits(results, "mvrs_results")) {
    abort("`results` must be a `mvrs_results` object.")
  }

  df <- results$estimates
  df$spec_id <- seq_len(nrow(df))
  df <- df[order(df$estimate), ]
  df$rank <- seq_len(nrow(df))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$rank, y = .data$estimate)) +
    ggplot2::geom_point(
      ggplot2::aes(colour = .data$significant),
      size = 1.5
    ) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::scale_colour_manual(
      values = c("TRUE" = "#2166AC", "FALSE" = "#B2182B"),
      labels = c("TRUE" = "p < 0.05", "FALSE" = "p >= 0.05"),
      name = NULL
    ) +
    ggplot2::labs(
      x = "Specification (ranked by estimate)",
      y = "Effect estimate",
      title = "Specification Curve"
    ) +
    ggplot2::theme_minimal()

  p
}

#' @export
summary.mvrs_results <- function(object, ...) {
  df <- object$estimates
  n_total <- nrow(df)
  n_sig <- sum(df$significant)
  pct_sig <- round(100 * n_sig / n_total, 1)
  med_est <- stats::median(df$estimate)
  iqr_est <- stats::IQR(df$estimate)

  cli_alert_info("Multiverse results: {.val {object$dataset_name}}")
  cli_alert_info("Question: {.val {object$question$outcome}} ~ {.val {object$question$predictor}}")
  cli_alert_info("Total specifications: {.val {n_total}}")
  cli_alert_info("Supporting (p < 0.05): {.val {n_sig}} ({pct_sig}%)")
  cli_alert_info("Median estimate: {.val {round(med_est, 4)}}")
  cli_alert_info("IQR of estimates: {.val {round(iqr_est, 4)}}")

  invisible(object)
}

#' @export
print.mvrs_results <- function(x, ...) {
  summary.mvrs_results(x, ...)
}
