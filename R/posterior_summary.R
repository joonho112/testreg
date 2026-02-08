#' Summarize a metareg_fit Object
#'
#' Produces a posterior summary table with mean, median, SD, and credible
#' intervals for all model parameters.
#'
#' @param object A `"metareg_fit"` object.
#' @param prob Width of the credible interval (default 0.95).
#' @param pars Character vector of parameter names to include in the summary.
#'   Defaults to `"beta"` and `"tau"`. Use `"all"` for all parameters.
#' @param ... Additional arguments (ignored).
#' @return A data frame with posterior summary statistics, printed as a table.
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' fit <- meta_reg_stan("yi", "vi", data = example_meta, chains = 2, iter = 1000)
#' summary(fit)
#' summary(fit, prob = 0.90)
#' }
summary.metareg_fit <- function(object, prob = 0.95, pars = NULL, ...) {
  draws <- object$draws
  all_vars <- posterior::variables(draws)

  if (is.null(pars)) {
    # Default: show beta and tau
    keep <- grep("^(beta|tau)$|^beta\\[", all_vars, value = TRUE)
  } else if (length(pars) == 1 && pars == "all") {
    keep <- all_vars
  } else {
    keep <- character(0)
    for (p in pars) {
      matched <- grep(paste0("^", p, "$|^", p, "\\["), all_vars, value = TRUE)
      keep <- c(keep, matched)
    }
  }

  if (length(keep) == 0) {
    stop("No matching parameters found.", call. = FALSE)
  }

  sub_draws <- posterior::subset_draws(draws, variable = keep)

  q_low_name <- paste0("q", 100 * (1 - prob) / 2)
  q_high_name <- paste0("q", 100 * (1 - (1 - prob) / 2))

  summary_fns <- list(
    mean   = mean,
    median = stats::median,
    sd     = stats::sd
  )
  summary_fns[[q_low_name]] <- function(x) {
    unname(stats::quantile(x, probs = (1 - prob) / 2))
  }
  summary_fns[[q_high_name]] <- function(x) {
    unname(stats::quantile(x, probs = 1 - (1 - prob) / 2))
  }

  smry <- do.call(posterior::summarise_draws, c(
    list(sub_draws),
    summary_fns,
    posterior::default_convergence_measures()
  ))

  # Add readable coefficient names for beta
  K <- object$model_info$K
  coef_names <- object$model_info$coef_names
  smry$label <- smry$variable
  for (k in seq_len(K)) {
    pat <- paste0("beta[", k, "]")
    smry$label[smry$variable == pat] <- coef_names[k]
  }

  class(smry) <- c("metareg_summary", class(smry))
  attr(smry, "prob") <- prob
  attr(smry, "backend") <- object$backend

  print_metareg_summary(smry)
  invisible(smry)
}

#' Print a metareg_summary
#' @param x A `metareg_summary` object.
#' @keywords internal
print_metareg_summary <- function(x) {
  prob <- attr(x, "prob")
  cat("Bayesian Meta-Regression Summary (", attr(x, "backend"), " backend)\n",
      sep = "")
  cat(prob * 100, "% credible intervals\n\n", sep = "")
  print(as.data.frame(x), row.names = FALSE)
  cat("\n")
}
