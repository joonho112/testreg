#' Posterior Predictive Check
#'
#' Generates posterior predictive check plots comparing observed data to
#' replicated datasets drawn from the posterior.
#'
#' @param object A `"metareg_fit"` object.
#' @param type Type of PPC plot: `"density"` (default), `"hist"`,
#'   `"scatter"`, or `"stat"`.
#' @param n_rep Number of replicated datasets to overlay (default 50).
#' @param ... Additional arguments passed to plotting functions.
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' fit <- meta_reg_stan("yi", "vi", data = example_meta, chains = 2, iter = 1000)
#' pp_check(fit)
#' pp_check(fit, type = "scatter")
#' }
pp_check <- function(object, ...) {
  UseMethod("pp_check")
}

#' @rdname pp_check
#' @export
pp_check.metareg_fit <- function(object, type = "density", n_rep = 50, ...) {
  y_obs <- object$data$y
  draws <- object$draws

  # Extract y_rep draws
  all_vars <- posterior::variables(draws)
  yrep_vars <- grep("^y_rep", all_vars, value = TRUE)
  if (length(yrep_vars) == 0) {
    stop("No y_rep variables found in posterior draws.", call. = FALSE)
  }

  yrep_draws <- posterior::subset_draws(draws, variable = yrep_vars)
  yrep_mat <- posterior::as_draws_matrix(yrep_draws)
  yrep_mat <- as.matrix(yrep_mat)

  # Subsample replications for plotting
  total_draws <- nrow(yrep_mat)
  idx <- sample(total_draws, min(n_rep, total_draws))
  yrep_sub <- yrep_mat[idx, , drop = FALSE]

  if (type == "density") {
    p <- .ppc_density(y_obs, yrep_sub)
  } else if (type == "scatter") {
    p <- .ppc_scatter(y_obs, yrep_sub)
  } else if (type == "stat") {
    p <- .ppc_stat(y_obs, yrep_mat)
  } else {
    stop("type must be 'density', 'scatter', or 'stat'.", call. = FALSE)
  }

  p
}

#' @keywords internal
.ppc_density <- function(y_obs, yrep_sub) {
  n_rep <- nrow(yrep_sub)
  N <- length(y_obs)

  # Build data frame for replications
  rep_df <- data.frame(
    value = as.vector(t(yrep_sub)),
    rep   = rep(seq_len(n_rep), each = N)
  )

  obs_df <- data.frame(value = y_obs)

  ggplot2::ggplot() +
    ggplot2::geom_density(
      data = rep_df,
      ggplot2::aes(x = .data$value, group = .data$rep),
      color = "skyblue", alpha = 0.3, linewidth = 0.3
    ) +
    ggplot2::geom_density(
      data = obs_df,
      ggplot2::aes(x = .data$value),
      color = "black", linewidth = 1.2
    ) +
    ggplot2::labs(
      title = "Posterior Predictive Check (Density Overlay)",
      x = "Effect Size", y = "Density"
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
.ppc_scatter <- function(y_obs, yrep_sub) {
  yrep_mean <- colMeans(yrep_sub)
  df <- data.frame(observed = y_obs, predicted = yrep_mean)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$observed, y = .data$predicted)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dashed",
                         color = "red") +
    ggplot2::labs(
      title = "Posterior Predictive Check (Scatter)",
      x = "Observed", y = "Mean Predicted"
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
.ppc_stat <- function(y_obs, yrep_mat) {
  rep_means <- apply(yrep_mat, 1, mean)
  obs_mean <- mean(y_obs)

  df <- data.frame(rep_mean = rep_means)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$rep_mean)) +
    ggplot2::geom_histogram(bins = 30, fill = "skyblue", color = "white") +
    ggplot2::geom_vline(xintercept = obs_mean, color = "red",
                        linewidth = 1.2, linetype = "dashed") +
    ggplot2::labs(
      title = "Posterior Predictive Check (Test Statistic: Mean)",
      x = "Replicated Mean", y = "Count"
    ) +
    ggplot2::theme_minimal()
}
