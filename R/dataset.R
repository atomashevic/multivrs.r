#' Create a multivrs dataset
#'
#' Wraps a data frame with an optional codebook into a multivrs dataset object
#' for use in the multiverse analysis pipeline.
#'
#' @param data A data frame or tibble.
#' @param codebook An optional codebook created by [mvrs_codebook()], or `NULL`.
#' @param name An optional name for the dataset.
#' @return An S3 object of class `mvrs_dataset`.
#' @export
mvrs_dataset <- function(data, codebook = NULL, name = NULL) {
  if (!is.data.frame(data)) {
    abort("`data` must be a data frame or tibble.")
  }
  if (!is.null(codebook) && !inherits(codebook, "mvrs_codebook")) {
    abort("`codebook` must be a `mvrs_codebook` object or NULL.")
  }

  structure(
    list(
      data = tibble::as_tibble(data),
      codebook = codebook,
      name = name %||% deparse(substitute(data))
    ),
    class = "mvrs_dataset"
  )
}

#' Create a codebook for variable metadata
#'
#' A codebook describes variables in a dataset: their types, measurement scales,
#' and missing value codes.
#'
#' @param ... Named lists describing variables. Each element should be a list
#'   with fields such as `type`, `scale`, and `missing`.
#' @return An S3 object of class `mvrs_codebook`.
#' @export
mvrs_codebook <- function(...) {
  entries <- list(...)
  if (length(entries) == 0) {
    abort("At least one variable entry is required.")
  }
  structure(entries, class = "mvrs_codebook")
}

#' @export
print.mvrs_dataset <- function(x, ...) {
  cli_alert_info("multivrs dataset: {.val {x$name}}")
  cli_alert_info("Dimensions: {nrow(x$data)} obs x {ncol(x$data)} vars")
  if (!is.null(x$codebook)) {
    cli_alert_info("Codebook: {length(x$codebook)} variable(s) described")
  } else {
    cli_alert_info("Codebook: not attached")
  }
  invisible(x)
}
