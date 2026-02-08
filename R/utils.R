#' Build Design Matrix for Meta-Regression
#'
#' Constructs a design matrix from moderator formulas or column names.
#'
#' @param mods A one-sided formula (e.g., `~ dosage + duration`) or `NULL`
#'   for an intercept-only model.
#' @param data A data frame containing the moderator variables.
#' @return A numeric design matrix with an intercept column.
#' @keywords internal
build_design_matrix <- function(mods, data) {
  if (is.null(mods)) {
    X <- matrix(1, nrow = nrow(data), ncol = 1)
    colnames(X) <- "(Intercept)"
  } else {
    if (inherits(mods, "formula")) {
      X <- model.matrix(mods, data = data)
    } else {
      stop("`mods` must be a one-sided formula or NULL.", call. = FALSE)
    }
  }
  X
}

#' Create a metareg_fit Object
#'
#' Constructor for the S3 class returned by fitting functions.
#'
#' @param draws A `draws_array` or `draws_matrix` object from the posterior
#'   package.
#' @param model_info A list with model metadata (parameter names, dimensions).
#' @param data A list with the data passed to the model.
#' @param fit The raw fit object from the backend.
#' @param backend Character string: `"stan"` or `"nimble"`.
#' @return An object of class `"metareg_fit"`.
#' @keywords internal
new_metareg_fit <- function(draws, model_info, data, fit, backend) {
  structure(
    list(
      draws      = draws,
      model_info = model_info,
      data       = data,
      fit        = fit,
      backend    = backend
    ),
    class = "metareg_fit"
  )
}

#' Print Method for metareg_fit
#'
#' @param x A `metareg_fit` object.
#' @param ... Additional arguments (ignored).
#' @return Invisibly returns `x`.
#' @export
print.metareg_fit <- function(x, ...) {
  cat("Bayesian Meta-Regression Fit\n")
  cat("Backend:", x$backend, "\n")
  cat("Number of studies:", x$model_info$N, "\n")
  cat("Number of predictors:", x$model_info$K, "\n")
  cat("Predictor names:", paste(x$model_info$coef_names, collapse = ", "), "\n")
  invisible(x)
}
