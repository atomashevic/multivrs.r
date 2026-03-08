test_that("mvrs_dataset creates correct class", {
  ds <- mvrs_dataset(mtcars, name = "mtcars")
  expect_s3_class(ds, "mvrs_dataset")
  expect_equal(ds$name, "mtcars")
  expect_equal(nrow(ds$data), 32)
  expect_equal(ncol(ds$data), 11)
})

test_that("mvrs_dataset rejects non-data-frame input", {
  expect_error(mvrs_dataset("not a data frame"), "must be a data frame")
})

test_that("mvrs_codebook attaches to dataset", {
  cb <- mvrs_codebook(
    mpg = list(type = "numeric", scale = "ratio"),
    cyl = list(type = "integer", scale = "ordinal")
  )
  ds <- mvrs_dataset(mtcars, codebook = cb, name = "mtcars")
  expect_s3_class(ds$codebook, "mvrs_codebook")
  expect_equal(length(ds$codebook), 2)
})

test_that("print.mvrs_dataset runs without error", {
  ds <- mvrs_dataset(mtcars, name = "mtcars")
  expect_output(print(ds), NA)
})

test_that("mvrs_enumerate_specs produces correct expansion", {
  d <- mvrs_decisions(
    mvrs_decision_node("outliers", c("keep", "remove")),
    mvrs_decision_node("model", c("lm", "glm", "robust"))
  )
  specs <- mvrs_enumerate_specs(d)
  expect_equal(nrow(specs), 6)
  expect_equal(ncol(specs), 2)
  expect_named(specs, c("outliers", "model"))
})
