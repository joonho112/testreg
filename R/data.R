#' Example Meta-Analysis Dataset
#'
#' A simulated meta-analysis dataset with 18 studies, including effect sizes,
#' sampling variances, and two moderator variables (dosage and duration).
#' The data are generated from a random-effects model with an overall
#' mean effect near 0.3.
#'
#' @format A data frame with 18 rows and 6 columns:
#' \describe{
#'   \item{study}{Character. Study identifier.}
#'   \item{yi}{Numeric. Observed effect size (e.g., standardized mean difference).}
#'   \item{vi}{Numeric. Sampling variance for the effect size.}
#'   \item{sei}{Numeric. Standard error (square root of `vi`).}
#'   \item{dosage}{Numeric. Dosage level moderator (range 10--100).}
#'   \item{duration}{Integer. Duration in weeks moderator (range 4--24).}
#' }
#' @examples
#' data(example_meta)
#' head(example_meta)
"example_meta"
