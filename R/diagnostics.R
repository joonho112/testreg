#' MCMC Convergence Diagnostics
#'
#' Reports Rhat, bulk effective sample size (ESS), and tail ESS for key
#' model parameters. Flags any potential convergence issues.
#'
#' @param object A `"metareg_fit"` object.
#' @param ... Additional arguments passed to methods.
#' @return A data frame with diagnostic statistics (invisibly).
#' @export
#' @examples
#' \dontrun{
#' data(example_meta)
#' fit <- meta_reg_stan("yi", "vi", data = example_meta, chains = 2, iter = 1000)
#' diagnose(fit)
#' }
diagnose <- function(object, ...) {
  UseMethod("diagnose")
}

#' @rdname diagnose
#' @param pars Character vector of parameter names to diagnose.
#'   Defaults to `"beta"` and `"tau"`.
#' @export
diagnose.metareg_fit <- function(object, pars = NULL, ...) {
  draws <- object$draws
  all_vars <- posterior::variables(draws)

  if (is.null(pars)) {
    keep <- grep("^(beta|tau)$|^beta\\[", all_vars, value = TRUE)
  } else {
    keep <- character(0)
    for (p in pars) {
      matched <- grep(paste0("^", p, "$|^", p, "\\["), all_vars, value = TRUE)
      keep <- c(keep, matched)
    }
  }

  sub_draws <- posterior::subset_draws(draws, variable = keep)
  diag_df <- posterior::summarise_draws(
    sub_draws,
    rhat       = posterior::rhat,
    ess_bulk   = posterior::ess_bulk,
    ess_tail   = posterior::ess_tail
  )

  # Flag issues
  issues <- character(0)
  bad_rhat <- diag_df$variable[!is.na(diag_df$rhat) & diag_df$rhat > 1.05]
  low_ess <- diag_df$variable[
    !is.na(diag_df$ess_bulk) & diag_df$ess_bulk < 400
  ]

  cat("MCMC Convergence Diagnostics\n")
  cat("============================\n\n")

  print(as.data.frame(diag_df), row.names = FALSE)
  cat("\n")

  if (length(bad_rhat) > 0) {
    issues <- c(issues,
      paste("WARNING: Rhat > 1.05 for:", paste(bad_rhat, collapse = ", "))
    )
  }
  if (length(low_ess) > 0) {
    issues <- c(issues,
      paste("WARNING: Bulk ESS < 400 for:", paste(low_ess, collapse = ", "))
    )
  }

  if (length(issues) > 0) {
    cat(paste(issues, collapse = "\n"), "\n")
    cat("\nConsider increasing iterations or reparameterizing.\n")
  } else {
    cat("All diagnostics look good. No convergence issues detected.\n")
  }

  invisible(diag_df)
}
