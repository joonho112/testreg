test_that("meta_reg_stan fits intercept-only model", {
  skip_if_not_installed("cmdstanr")
  skip_on_cran()

  data(example_meta, package = "testreg")
  fit <- meta_reg_stan(
    yi = "yi", vi = "vi", data = example_meta,
    chains = 2, iter = 1000, warmup = 500
  )

  expect_s3_class(fit, "metareg_fit")
  expect_equal(fit$backend, "cmdstanr")
  expect_equal(fit$model_info$N, 18)
  expect_equal(fit$model_info$K, 1)
  expect_true(!is.null(fit$draws))
})

test_that("meta_reg_stan fits model with moderators", {
  skip_if_not_installed("cmdstanr")
  skip_on_cran()

  data(example_meta, package = "testreg")
  fit <- meta_reg_stan(
    yi = "yi", vi = "vi", mods = ~ dosage,
    data = example_meta,
    chains = 2, iter = 1000, warmup = 500
  )

  expect_s3_class(fit, "metareg_fit")
  expect_equal(fit$model_info$K, 2)
  expect_equal(fit$model_info$coef_names, c("(Intercept)", "dosage"))
})

test_that("meta_reg_stan rejects bad backend", {
  data(example_meta, package = "testreg")
  expect_error(
    meta_reg_stan("yi", "vi", data = example_meta, backend = "jags"),
    "backend must be"
  )
})
