#' Load ESS data from local files
#'
#' Reads ESS data from a local file and wraps it in an [`mvrs_dataset`]
#' object so it can be used in multivrs workflows.
#'
#' @param path Path to an ESS data file (`.csv`, `.parquet`, `.sav`, `.dta`).
#' @param codebook_path Optional path to a JSON codebook file.
#' @param recode_missing Whether to recode standard ESS missing codes to `NA`.
#' @param name Optional dataset name.
#' @return A `mvrs_dataset` object.
#' @export
mvrs_load_ess <- function(path, codebook_path = NULL, recode_missing = TRUE, name = NULL) {
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    abort("`path` must be a non-empty string.")
  }
  if (!file.exists(path)) {
    abort(paste0("File does not exist: ", path))
  }

  ext <- tolower(tools::file_ext(path))
  data <- switch(
    ext,
    csv = utils::read.csv(path, stringsAsFactors = FALSE),
    parquet = {
      if (!requireNamespace("arrow", quietly = TRUE)) {
        abort("Reading parquet requires the `arrow` package.")
      }
      as.data.frame(arrow::read_parquet(path))
    },
    sav = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        abort("Reading .sav files requires the `haven` package.")
      }
      as.data.frame(haven::read_sav(path))
    },
    dta = {
      if (!requireNamespace("haven", quietly = TRUE)) {
        abort("Reading .dta files requires the `haven` package.")
      }
      as.data.frame(haven::read_dta(path))
    },
    abort(paste0("Unsupported ESS file extension: .", ext))
  )

  if (isTRUE(recode_missing)) {
    missing_codes <- c(6, 7, 8, 9, 66, 77, 88, 99, 666, 777, 888, 999)
    for (nm in names(data)) {
      if (is.numeric(data[[nm]])) {
        data[[nm]][data[[nm]] %in% missing_codes] <- NA_real_
      }
    }
  }

  codebook <- NULL
  if (!is.null(codebook_path)) {
    if (!file.exists(codebook_path)) {
      abort(paste0("Codebook file does not exist: ", codebook_path))
    }
    if (!requireNamespace("jsonlite", quietly = TRUE)) {
      abort("Reading JSON codebook requires the `jsonlite` package.")
    }
    raw_codebook <- jsonlite::fromJSON(codebook_path, simplifyVector = FALSE)
    codebook <- structure(raw_codebook, class = "mvrs_codebook")
  }

  mvrs_dataset(
    data = data,
    codebook = codebook,
    name = name %||% basename(path)
  )
}

pick_field <- function(x, ...) {
  keys <- c(...)
  for (k in keys) {
    if (!is.null(x[[k]])) return(x[[k]])
  }
  NULL
}

build_variant_grid <- function(model_family, random_effects, max_variants) {
  base <- list(
    list(
      id = "v1",
      model_family = model_family,
      random_effects = random_effects,
      robust_se = "none",
      optimizer = "bobyqa",
      reml = FALSE
    )
  )
  if (identical(model_family, "lm")) {
    base <- c(
      base,
      list(
        list(id = "v2", model_family = "lm", random_effects = "none", robust_se = "HC3", optimizer = "bobyqa", reml = FALSE),
        list(id = "v3", model_family = "lm", random_effects = "none", robust_se = "HC1", optimizer = "bobyqa", reml = FALSE),
        list(id = "v4", model_family = "lm", random_effects = "none", robust_se = "none", optimizer = "bobyqa", reml = FALSE)
      )
    )
  } else {
    base <- c(
      base,
      list(
        list(id = "v2", model_family = "lmer", random_effects = random_effects, robust_se = "none", optimizer = "nloptwrap", reml = FALSE),
        list(id = "v3", model_family = "lmer", random_effects = random_effects, robust_se = "none", optimizer = "bobyqa", reml = TRUE),
        list(id = "v4", model_family = "lmer", random_effects = if (identical(random_effects, "slope")) "intercept" else random_effects, robust_se = "none", optimizer = "bobyqa", reml = FALSE)
      )
    )
  }
  base[seq_len(min(length(base), max_variants))]
}

run_one_variant <- function(df, outcome, predictor, controls, direction, weight_col, country_col, variant) {
  tick <- function(x) paste0("`", gsub("`", "", x), "`")
  row_for <- function(tbl, name) {
    idx <- which(rownames(tbl) %in% c(name, tick(name)))
    if (length(idx) == 0) return(NA_integer_)
    idx[[1]]
  }

  rhs <- paste(c(tick(predictor), vapply(controls, tick, character(1))), collapse = " + ")
  if (!nzchar(rhs)) rhs <- "1"
  base_formula <- stats::as.formula(paste(tick(outcome), "~", rhs))

  warnings <- character(0)
  estimate <- NA_real_
  std_error <- NA_real_
  p_value <- NA_real_
  model_used <- variant$model_family

  weights <- if (!is.null(weight_col) && weight_col %in% names(df)) df[[weight_col]] else NULL

  if (identical(model_used, "lmer") &&
      !is.null(country_col) &&
      !identical(variant$random_effects, "none") &&
      requireNamespace("lme4", quietly = TRUE)) {
    random_term <- if (identical(variant$random_effects, "slope")) {
      paste0("(1 + ", tick(predictor), " | ", tick(country_col), ")")
    } else {
      paste0("(1 | ", tick(country_col), ")")
    }
    formula <- stats::as.formula(paste(deparse(base_formula), "+", random_term))
    fit <- try(
      lme4::lmer(
        formula = formula,
        data = df,
        weights = weights,
        REML = isTRUE(variant$reml),
        control = lme4::lmerControl(optimizer = variant$optimizer)
      ),
      silent = TRUE
    )
    if (inherits(fit, "try-error")) {
      warnings <- c(warnings, "lmer-fit-failed")
    } else {
      coefs <- summary(fit)$coefficients
      idx <- row_for(coefs, predictor)
      if (is.na(idx)) {
        warnings <- c(warnings, "predictor-coef-missing")
      } else {
        estimate <- as.numeric(coefs[idx, 1])
        std_error <- as.numeric(coefs[idx, 2])
      }
    }
  } else {
    if (identical(model_used, "lmer")) {
      warnings <- c(warnings, "lmer-unavailable-fallback-lm")
      model_used <- "lm"
    }
    fit <- try(stats::lm(formula = base_formula, data = df, weights = weights), silent = TRUE)
    if (inherits(fit, "try-error")) {
      warnings <- c(warnings, "lm-fit-failed")
    } else {
      coefs <- summary(fit)$coefficients
      if (!identical(variant$robust_se, "none") &&
          requireNamespace("sandwich", quietly = TRUE) &&
          requireNamespace("lmtest", quietly = TRUE)) {
        coefs <- lmtest::coeftest(fit, vcov. = sandwich::vcovHC(fit, type = variant$robust_se))
      }
      idx <- row_for(coefs, predictor)
      if (is.na(idx)) {
        warnings <- c(warnings, "predictor-coef-missing")
      } else {
        estimate <- as.numeric(coefs[idx, 1])
        std_error <- as.numeric(coefs[idx, 2])
        if (ncol(coefs) >= 4) {
          p_value <- as.numeric(coefs[idx, 4])
        }
      }
    }
  }

  conclusion <- "inconclusive"
  if (is.finite(estimate)) {
    supports <- FALSE
    contradicts <- FALSE
    if (is.finite(p_value) && p_value < 0.05) {
      supports <- (identical(direction, "positive") && estimate > 0) ||
        (identical(direction, "negative") && estimate < 0)
      contradicts <- (identical(direction, "positive") && estimate < 0) ||
        (identical(direction, "negative") && estimate > 0)
    }
    if (supports) conclusion <- "supports-hypothesis"
    if (contradicts) conclusion <- "contradicts-hypothesis"
  } else {
    conclusion <- "failed"
  }

  list(
    variant_id = variant$id,
    model_family = model_used,
    effect_estimate = estimate,
    std_error = std_error,
    p_value = p_value,
    conclusion = conclusion,
    warnings = unique(warnings)
  )
}

#' Run a capped per-agent model grid in R
#'
#' Executes a bounded model variant grid for one analyst decision signature.
#'
#' @param data A data frame or `mvrs_dataset`.
#' @param selection Named list with decision selections from the orchestrator.
#' @param question Optional `mvrs_question` or list containing direction/hypothesis.
#' @param max_variants Maximum number of model variants to run.
#' @return A list containing normalized aggregate fields and per-variant outputs.
#' @export
mvrs_run_agent_grid <- function(data, selection, question = NULL, max_variants = 4) {
  if (inherits(data, "mvrs_dataset")) {
    data <- data$data
  }
  if (!is.data.frame(data)) {
    abort("`data` must be a data frame or mvrs_dataset.")
  }
  if (!is.list(selection)) {
    abort("`selection` must be a list.")
  }

  outcome <- pick_field(selection, "outcome")
  predictor <- pick_field(selection, "predictor")
  controls <- pick_field(selection, "controls")
  sample_filter <- pick_field(selection, "sample_filter", "sampleFilter")
  missing_strategy <- pick_field(selection, "missing_strategy", "missingStrategy")
  weight_strategy <- pick_field(selection, "weight_strategy", "weightStrategy")
  model_family <- pick_field(selection, "model_family", "modelFamily")
  random_effects <- pick_field(selection, "random_effects", "randomEffects")

  if (!is.character(outcome) || !nzchar(outcome)) abort("`selection$outcome` is required.")
  if (!is.character(predictor) || !nzchar(predictor)) abort("`selection$predictor` is required.")
  if (is.null(controls)) controls <- character(0)
  controls <- as.character(controls)
  if (is.null(sample_filter) || !nzchar(sample_filter)) sample_filter <- "TRUE"
  if (is.null(missing_strategy) || !nzchar(missing_strategy)) missing_strategy <- "listwise"
  if (is.null(weight_strategy) || !nzchar(weight_strategy)) weight_strategy <- "none"
  if (is.null(model_family) || !nzchar(model_family)) model_family <- "lm"
  if (is.null(random_effects) || !nzchar(random_effects)) random_effects <- "none"

  df <- tibble::as_tibble(data)
  warnings <- character(0)

  if (!identical(sample_filter, "TRUE")) {
    keep <- try(eval(parse(text = sample_filter), envir = as.data.frame(df)), silent = TRUE)
    if (inherits(keep, "try-error")) {
      warnings <- c(warnings, "sample-filter-failed")
    } else {
      keep <- as.logical(keep)
      keep[is.na(keep)] <- FALSE
      df <- df[keep, , drop = FALSE]
    }
  }

  required <- unique(c(outcome, predictor, controls))
  missing_vars <- required[!required %in% names(df)]
  if (length(missing_vars) > 0) {
    abort(paste0("Variables not found: ", paste(missing_vars, collapse = ", ")))
  }

  if (identical(missing_strategy, "listwise")) {
    df <- df[stats::complete.cases(df[, required, drop = FALSE]), , drop = FALSE]
  } else if (identical(missing_strategy, "mean-impute")) {
    for (nm in required) {
      if (is.numeric(df[[nm]])) {
        m <- mean(df[[nm]], na.rm = TRUE)
        if (is.finite(m)) df[[nm]][is.na(df[[nm]])] <- m
      }
    }
    df <- df[stats::complete.cases(df[, required, drop = FALSE]), , drop = FALSE]
  }

  if (nrow(df) < 10) {
    abort("Not enough rows after preprocessing.")
  }

  weight_col <- switch(
    weight_strategy,
    "design" = if ("dweight" %in% names(df)) "dweight" else NULL,
    "post-strat" = if ("pspwght" %in% names(df)) "pspwght" else NULL,
    "analysis" = if ("anweight" %in% names(df)) "anweight" else NULL,
    NULL
  )
  if (!identical(weight_strategy, "none") && is.null(weight_col)) {
    warnings <- c(warnings, "weight-column-missing")
  }

  country_col <- if ("cntry" %in% names(df)) "cntry" else if ("country" %in% names(df)) "country" else NULL

  direction <- "positive"
  if (inherits(question, "mvrs_question") && !is.null(question$direction)) {
    direction <- question$direction
  } else if (is.list(question) && !is.null(question$direction)) {
    direction <- as.character(question$direction[[1]])
  } else if (is.list(question) && !is.null(question$hypothesis)) {
    hypothesis <- tolower(as.character(question$hypothesis[[1]]))
    if (grepl("negative|lower", hypothesis)) direction <- "negative"
  }

  variants <- build_variant_grid(
    model_family = as.character(model_family),
    random_effects = as.character(random_effects),
    max_variants = max(1, as.integer(max_variants))
  )

  variant_results <- lapply(
    variants,
    function(variant) run_one_variant(
      df = as.data.frame(df),
      outcome = outcome,
      predictor = predictor,
      controls = controls,
      direction = direction,
      weight_col = weight_col,
      country_col = country_col,
      variant = variant
    )
  )

  estimates <- vapply(
    variant_results,
    function(x) if (is.finite(x$effect_estimate)) x$effect_estimate else NA_real_,
    numeric(1)
  )
  estimates <- estimates[is.finite(estimates)]
  effect_estimate <- if (length(estimates) > 0) stats::median(estimates) else NA_real_
  support_share <- mean(vapply(variant_results, function(x) identical(x$conclusion, "supports-hypothesis"), logical(1)))
  if (!is.finite(support_share)) support_share <- 0

  conclusion <- if (!is.finite(effect_estimate)) {
    "run-error"
  } else if (support_share >= 0.6) {
    "supports-hypothesis"
  } else if (support_share <= 0.2) {
    "weak-support"
  } else {
    "inconclusive"
  }

  list(
    effect_estimate = if (is.finite(effect_estimate)) effect_estimate else NULL,
    effect_metric = "coefficient",
    conclusion = conclusion,
    warnings = unique(c(warnings, unlist(lapply(variant_results, function(x) x$warnings)))),
    support_share = support_share,
    variants = variant_results
  )
}

#' Emit normalized agent result as JSON
#'
#' Converts a result list from [mvrs_run_agent_grid()] into a JSON string.
#'
#' @param result A list returned by [mvrs_run_agent_grid()].
#' @param analyst_id Optional analyst identifier.
#' @param question_id Optional question identifier.
#' @return A JSON string.
#' @export
mvrs_emit_agent_result <- function(result, analyst_id = NULL, question_id = NULL) {
  if (!is.list(result)) {
    abort("`result` must be a list.")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    abort("`jsonlite` is required to emit JSON output.")
  }

  payload <- result
  payload$analyst_id <- analyst_id %||% payload$analyst_id
  payload$question_id <- question_id %||% payload$question_id

  jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
}
