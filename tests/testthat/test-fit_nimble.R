test_that("meta_reg_nimble fits intercept-only model", {
  skip_if_not_installed("nimble")
  skip_on_cran()

  data(example_meta, package = "testreg")
  fit <- meta_reg_nimble(
    yi = "yi", vi = "vi", data = example_meta,
    chains = 2, iter = 1000, warmup = 500
  )

  expect_s3_class(fit, "metareg_fit")
  expect_equal(fit$backend, "nimble")
  expect_equal(fit$model_info$N, 18)
  expect_equal(fit$model_info$K, 1)
  expect_true(!is.null(fit$draws))
})

test_that("meta_reg_nimble fits model with moderators", {
  skip_if_not_installed("nimble")
  skip_on_cran()

  data(example_meta, package = "testreg")
  fit <- meta_reg_nimble(
    yi = "yi", vi = "vi", mods = ~ dosage,
    data = example_meta,
    chains = 2, iter = 1000, warmup = 500
  )

  expect_s3_class(fit, "metareg_fit")
  expect_equal(fit$model_info$K, 2)
  expect_equal(fit$model_info$coef_names, c("(Intercept)", "dosage"))
})
