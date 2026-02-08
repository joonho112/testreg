test_that("summary.metareg_fit produces summary table", {
  skip_if_not_installed("cmdstanr")
  skip_on_cran()

  data(example_meta, package = "testreg")
  fit <- meta_reg_stan(
    yi = "yi", vi = "vi", data = example_meta,
    chains = 2, iter = 1000, warmup = 500
  )

  smry <- summary(fit)
  expect_true(is.data.frame(smry))
  expect_true("mean" %in% names(smry))
  expect_true("median" %in% names(smry))
  expect_true("sd" %in% names(smry))
})

test_that("summary.metareg_fit respects prob argument", {
  skip_if_not_installed("cmdstanr")
  skip_on_cran()

  data(example_meta, package = "testreg")
  fit <- meta_reg_stan(
    yi = "yi", vi = "vi", data = example_meta,
    chains = 2, iter = 1000, warmup = 500
  )

  smry90 <- summary(fit, prob = 0.90)
  expect_true("q5" %in% names(smry90))
  expect_true("q95" %in% names(smry90))
})

test_that("pp_check produces a ggplot", {
  skip_if_not_installed("cmdstanr")
  skip_on_cran()

  data(example_meta, package = "testreg")
  fit <- meta_reg_stan(
    yi = "yi", vi = "vi", data = example_meta,
    chains = 2, iter = 1000, warmup = 500
  )

  p <- pp_check(fit)
  expect_s3_class(p, "ggplot")

  p2 <- pp_check(fit, type = "scatter")
  expect_s3_class(p2, "ggplot")
})
