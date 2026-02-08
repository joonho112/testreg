#' Fit Bayesian Meta-Regression via Stan
#'
#' Fits a random-effects meta-regression model using Stan (cmdstanr preferred,
#' rstan as fallback).
#'
#' @param yi Character string naming the effect-size column, or a numeric
#'   vector of observed effect sizes.
#' @param vi Character string naming the sampling-variance column, or a
#'   numeric vector of sampling variances.
#' @param mods A one-sided formula for moderators (e.g., `~ dosage + duration`)
#'   or `NULL` for an intercept-only model.
#' @param data A data frame containing the variables.
#' @param chains Number of MCMC chains (default 4).
#' @param iter Number of total iterations per chain (default 2000).
#' @param warmup Number of warmup iterations per chain (default 1000).
#' @param seed Random seed for reproducibility.
#' @param backend Character: `"cmdstanr"` (default) or `"rstan"`.
#' @return An object of class `"metareg_fit"`.
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' fit <- meta_reg_stan(yi = "yi", vi = "vi", mods = ~ dosage,
#'                      data = example_meta, chains = 2, iter = 1000)
#' summary(fit)
#' }
meta_reg_stan <- function(yi, vi, mods = NULL, data,
                          chains = 4, iter = 2000, warmup = 1000,
                          seed = 12345, backend = "cmdstanr") {

  # Resolve yi and vi
  if (is.character(yi)) yi <- data[[yi]]
  if (is.character(vi)) vi <- data[[vi]]
  se <- sqrt(vi)

  # Build design matrix
  X <- build_design_matrix(mods, data)
  N <- length(yi)
  K <- ncol(X)

  stan_data <- list(N = N, K = K, y = yi, se = se, X = X)

  stan_file <- system.file("stan", "meta_regression.stan", package = "testreg")
  if (stan_file == "") {
    stop("Stan model file not found. Is the package installed?", call. = FALSE)
  }

  if (backend == "cmdstanr") {
    if (!requireNamespace("cmdstanr", quietly = TRUE)) {
      stop("cmdstanr is required but not installed.", call. = FALSE)
    }
    mod <- cmdstanr::cmdstan_model(stan_file)
    fit_raw <- mod$sample(
      data = stan_data,
      chains = chains,
      iter_sampling = iter - warmup,
      iter_warmup = warmup,
      seed = seed,
      refresh = 0
    )
    draws <- posterior::as_draws_array(fit_raw$draws())
  } else if (backend == "rstan") {
    if (!requireNamespace("rstan", quietly = TRUE)) {
      stop("rstan is required but not installed.", call. = FALSE)
    }
    fit_raw <- rstan::stan(
      file = stan_file,
      data = stan_data,
      chains = chains,
      iter = iter,
      warmup = warmup,
      seed = seed,
      refresh = 0
    )
    draws <- posterior::as_draws_array(fit_raw)
  } else {
    stop("backend must be 'cmdstanr' or 'rstan'.", call. = FALSE)
  }

  model_info <- list(
    N = N, K = K,
    coef_names = colnames(X),
    formula = mods
  )

  new_metareg_fit(
    draws      = draws,
    model_info = model_info,
    data       = stan_data,
    fit        = fit_raw,
    backend    = backend
  )
}
