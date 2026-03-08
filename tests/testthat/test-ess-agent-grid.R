test_that("mvrs_load_ess reads CSV and returns mvrs_dataset", {
  tf <- tempfile(fileext = ".csv")
  on.exit(unlink(tf), add = TRUE)

  write.csv(
    data.frame(
      gincdif = c(1, 2, 3),
      imueclt = c(2, 3, 4),
      agea = c(30, 40, 50)
    ),
    tf,
    row.names = FALSE
  )

  ds <- mvrs_load_ess(tf, recode_missing = FALSE)
  expect_s3_class(ds, "mvrs_dataset")
  expect_equal(nrow(ds$data), 3)
})

test_that("mvrs_run_agent_grid returns normalized result structure", {
  set.seed(42)
  n <- 120
  dat <- data.frame(
    gincdif = rnorm(n),
    imueclt = rnorm(n),
    agea = sample(18:80, n, replace = TRUE),
    gndr = sample(0:1, n, replace = TRUE),
    dweight = runif(n, 0.5, 1.5)
  )
  dat$gincdif <- 0.4 * dat$imueclt + 0.02 * dat$agea + rnorm(n, sd = 0.5)

  selection <- list(
    outcome = "gincdif",
    predictor = "imueclt",
    controls = c("agea", "gndr"),
    sampleFilter = "TRUE",
    missingStrategy = "listwise",
    weightStrategy = "design",
    modelFamily = "lm",
    randomEffects = "none"
  )

  res <- mvrs_run_agent_grid(dat, selection, max_variants = 2)
  expect_type(res, "list")
  expect_true("conclusion" %in% names(res))
  expect_true("variants" %in% names(res))
  expect_length(res$variants, 2)
})

test_that("mvrs_emit_agent_result returns JSON string", {
  skip_if_not_installed("jsonlite")
  payload <- list(
    effect_estimate = 0.12,
    effect_metric = "coefficient",
    conclusion = "inconclusive",
    warnings = character(0),
    variants = list()
  )
  out <- mvrs_emit_agent_result(payload, analyst_id = "a1", question_id = "q1")
  expect_type(out, "character")
  expect_match(out, "analyst_id")
  expect_match(out, "question_id")
})
