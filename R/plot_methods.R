#' Plot Method for metareg_fit
#'
#' Produces various visualizations for meta-regression results.
#'
#' @param x A `"metareg_fit"` object.
#' @param type Character string specifying the plot type: `"forest"` (default),
#'   `"caterpillar"`, `"trace"`, or `"density"`.
#' @param pars Parameter names to plot. Defaults to beta and tau for
#'   caterpillar, trace, and density plots.
#' @param ... Additional arguments (ignored).
#' @return A ggplot object.
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' fit <- meta_reg_stan("yi", "vi", data = example_meta, chains = 2, iter = 1000)
#' plot(fit, type = "forest")
#' plot(fit, type = "caterpillar")
#' plot(fit, type = "trace")
#' }
plot.metareg_fit <- function(x, type = "forest", pars = NULL, ...) {
  type <- match.arg(type, c("forest", "caterpillar", "trace", "density"))

  switch(type,
    forest      = .plot_forest(x),
    caterpillar = .plot_caterpillar(x, pars),
    trace       = .plot_trace(x, pars),
    density     = .plot_density(x, pars)
  )
}

#' @keywords internal
.get_param_vars <- function(object, pars) {
  all_vars <- posterior::variables(object$draws)
  if (is.null(pars)) {
    grep("^(beta|tau)$|^beta\\[", all_vars, value = TRUE)
  } else {
    keep <- character(0)
    for (p in pars) {
      matched <- grep(paste0("^", p, "$|^", p, "\\["), all_vars, value = TRUE)
      keep <- c(keep, matched)
    }
    keep
  }
}

#' @keywords internal
.plot_forest <- function(object) {
  y_obs <- object$data$y
  se <- object$data$se
  N <- object$model_info$N

  study_labels <- paste0("Study ", seq_len(N))

  df <- data.frame(
    study = factor(study_labels, levels = rev(study_labels)),
    yi    = y_obs,
    lower = y_obs - 1.96 * se,
    upper = y_obs + 1.96 * se
  )

  # Overall pooled estimate from beta[1]
  beta1_draws <- posterior::subset_draws(object$draws, variable = "beta[1]")
  beta1_vec <- as.vector(posterior::as_draws_matrix(beta1_draws))
  pooled <- mean(beta1_vec)
  pooled_lower <- stats::quantile(beta1_vec, 0.025)
  pooled_upper <- stats::quantile(beta1_vec, 0.975)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$yi, y = .data$study)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$lower, xmax = .data$upper),
      height = 0.2
    ) +
    ggplot2::geom_vline(xintercept = pooled, linetype = "dashed",
                        color = "blue") +
    ggplot2::annotate(
      "rect",
      xmin = pooled_lower, xmax = pooled_upper,
      ymin = 0.2, ymax = N + 0.8,
      fill = "blue", alpha = 0.08
    ) +
    ggplot2::labs(
      title = "Forest Plot",
      x = "Effect Size", y = ""
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
.plot_caterpillar <- function(object, pars) {
  vars <- .get_param_vars(object, pars)
  sub_draws <- posterior::subset_draws(object$draws, variable = vars)
  smry <- posterior::summarise_draws(
    sub_draws,
    mean   = mean,
    q5     = ~stats::quantile(.x, 0.05),
    q25    = ~stats::quantile(.x, 0.25),
    q75    = ~stats::quantile(.x, 0.75),
    q95    = ~stats::quantile(.x, 0.95)
  )

  smry$variable <- factor(smry$variable, levels = rev(smry$variable))

  ggplot2::ggplot(smry, ggplot2::aes(x = .data$mean, y = .data$variable)) +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$q5, xmax = .data$q95),
      height = 0.15, linewidth = 0.4
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = .data$q25, xmax = .data$q75),
      height = 0.15, linewidth = 1.0
    ) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    ggplot2::labs(
      title = "Caterpillar Plot",
      x = "Estimate", y = ""
    ) +
    ggplot2::theme_minimal()
}

#' @keywords internal
.plot_trace <- function(object, pars) {
  vars <- .get_param_vars(object, pars)
  sub_draws <- posterior::subset_draws(object$draws, variable = vars)
  draws_df <- posterior::as_draws_df(sub_draws)

  # Reshape to long format
  id_cols <- c(".chain", ".iteration", ".draw")
  value_cols <- setdiff(names(draws_df), id_cols)

  long_df <- stats::reshape(
    as.data.frame(draws_df),
    direction = "long",
    varying = value_cols,
    v.names = "value",
    timevar = "parameter",
    times = value_cols,
    idvar = ".draw"
  )

  long_df$.chain <- factor(long_df$.chain)

  ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = .data$.iteration, y = .data$value,
                 color = .data$.chain)
  ) +
    ggplot2::geom_line(alpha = 0.6, linewidth = 0.3) +
    ggplot2::facet_wrap(~ parameter, scales = "free_y") +
    ggplot2::labs(title = "Trace Plots", x = "Iteration", y = "Value",
                  color = "Chain") +
    ggplot2::theme_minimal()
}

#' @keywords internal
.plot_density <- function(object, pars) {
  vars <- .get_param_vars(object, pars)
  sub_draws <- posterior::subset_draws(object$draws, variable = vars)
  draws_df <- posterior::as_draws_df(sub_draws)

  id_cols <- c(".chain", ".iteration", ".draw")
  value_cols <- setdiff(names(draws_df), id_cols)

  long_df <- stats::reshape(
    as.data.frame(draws_df),
    direction = "long",
    varying = value_cols,
    v.names = "value",
    timevar = "parameter",
    times = value_cols,
    idvar = ".draw"
  )

  ggplot2::ggplot(long_df, ggplot2::aes(x = .data$value)) +
    ggplot2::geom_density(fill = "skyblue", alpha = 0.5) +
    ggplot2::facet_wrap(~ parameter, scales = "free") +
    ggplot2::labs(title = "Posterior Density Plots", x = "Value",
                  y = "Density") +
    ggplot2::theme_minimal()
}
