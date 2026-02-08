# Suppress R CMD check NOTEs for NIMBLE DSL symbols
utils::globalVariables(c("inprod", "u", "y", "dnorm"))

#' Fit Bayesian Meta-Regression via NIMBLE
#'
#' Fits a random-effects meta-regression model using NIMBLE.
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
#' @param warmup Number of warmup/burnin iterations per chain (default 1000).
#' @param seed Random seed for reproducibility.
#' @param ... Additional arguments (currently unused).
#' @return An object of class `"metareg_fit"`.
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' fit <- meta_reg_nimble(yi = "yi", vi = "vi", mods = ~ dosage,
#'                        data = example_meta, chains = 2, iter = 1000)
#' summary(fit)
#' }
meta_reg_nimble <- function(yi, vi, mods = NULL, data,
                            chains = 4, iter = 2000, warmup = 1000,
                            seed = 12345, ...) {

  if (!requireNamespace("nimble", quietly = TRUE)) {
    stop("nimble is required but not installed.", call. = FALSE)
  }

  # NIMBLE requires attachment to the search path because its reference class
  # methods call functions like getNimbleOption() in the global environment
  if (!"package:nimble" %in% search()) {
    suppressPackageStartupMessages(
      attachNamespace("nimble")
    )
  }

  # Resolve yi and vi
  if (is.character(yi)) yi <- data[[yi]]
  if (is.character(vi)) vi <- data[[vi]]
  se <- sqrt(vi)

  # Build design matrix
  X_orig <- build_design_matrix(mods, data)
  N <- length(yi)
  K_orig <- ncol(X_orig)

  # NIMBLE's C++ code generator has a bug with `inprod(X[i, 1:1], beta[1:1])`
  # when K=1 (treats 1:1 as scalar). Pad with a zero column so K >= 2.
  if (K_orig == 1L) {
    X <- cbind(X_orig, 0)
    K <- 2L
  } else {
    X <- X_orig
    K <- K_orig
  }

  nimble_code <- nimble::nimbleCode({
    for (k in 1:K) {
      beta[k] ~ dnorm(0, sd = 10)
    }
    tau ~ T(dt(0, 1, 1), 0, )

    for (i in 1:N) {
      u[i] ~ dnorm(0, sd = tau)
      mu[i] <- inprod(X[i, 1:K], beta[1:K]) + u[i]
      y[i] ~ dnorm(mu[i], sd = se[i])
      y_rep[i] ~ dnorm(mu[i], sd = se[i])
    }
  })

  nimble_data <- list(y = yi, se = se, X = X)
  nimble_constants <- list(N = N, K = K)

  inits_fn <- function() {
    list(
      beta  = rnorm(K, 0, 1),
      tau   = abs(rnorm(1, 0, 0.5)),
      u     = rnorm(N, 0, 0.1),
      y_rep = rnorm(N, mean(yi), sd(yi))
    )
  }

  monitors <- c("beta", "tau", "u", "y_rep")

  set.seed(seed)

  # Suppress NIMBLE's verbose compilation messages
  samples_list <- suppressWarnings(suppressMessages(
    nimble::nimbleMCMC(
      code = nimble_code,
      constants = nimble_constants,
      data = nimble_data,
      inits = inits_fn,
      monitors = monitors,
      nchains = chains,
      niter = iter,
      nburnin = warmup,
      setSeed = seed + seq_len(chains) - 1L,
      summary = FALSE,
      WAIC = FALSE,
      progressBar = FALSE
    )
  ))

  # Convert to posterior draws_array
  n_samples <- iter - warmup
  par_names <- colnames(samples_list[[1]])

  # If we padded the design matrix, drop the dummy beta column from results
  if (K_orig < K) {
    dummy_par <- paste0("beta[", K, "]")
    par_names <- par_names[par_names != dummy_par]
    for (ch in seq_along(samples_list)) {
      samples_list[[ch]] <- samples_list[[ch]][, par_names, drop = FALSE]
    }
  }

  draws_arr <- array(
    NA_real_,
    dim = c(n_samples, chains, length(par_names)),
    dimnames = list(NULL, paste0("chain:", seq_len(chains)), par_names)
  )
  for (ch in seq_len(chains)) {
    draws_arr[, ch, ] <- samples_list[[ch]]
  }
  draws <- posterior::as_draws_array(draws_arr)

  model_info <- list(
    N = N, K = K_orig,
    coef_names = colnames(X_orig),
    formula = mods
  )

  stan_data <- list(N = N, K = K_orig, y = yi, se = se, X = X_orig)

  new_metareg_fit(
    draws      = draws,
    model_info = model_info,
    data       = stan_data,
    fit        = samples_list,
    backend    = "nimble"
  )
}
