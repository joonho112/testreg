test_that("diagnose works on a metareg_fit object", {
  skip_if_not_installed("cmdstanr")
  skip_on_cran()

  data(example_meta, package = "testreg")
  fit <- meta_reg_stan(
    yi = "yi", vi = "vi", data = example_meta,
    chains = 2, iter = 1000, warmup = 500
  )

  diag_out <- diagnose(fit)
  expect_true(is.data.frame(diag_out))
  expect_true("rhat" %in% names(diag_out))
  expect_true("ess_bulk" %in% names(diag_out))
  expect_true("ess_tail" %in% names(diag_out))
})
