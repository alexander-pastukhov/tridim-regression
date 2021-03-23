#' Computes R-squared using Bayesian R-squared approach.
#' For detail refer to:
#' Andrew Gelman, Ben Goodrich, Jonah Gabry, and Aki Vehtari (2018).
#' R-squared for Bayesian regression models. The American Statistician,
#' doi:10.1080/00031305.2018.1549100.
#'
#' @name R2
#'
#' @param object An object of class [tridim_transformation][tridim_transformation-class()]
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#' @param ... Unused.
#'
#' @return vector of values or a data.frame with summary
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'   NakayaData, transformation = 'euclidean')
#' R2(euc2)
#' }
R2.tridim_transformation <- function(object, summary=TRUE, probs=c(0.055, 0.945), ...){
  # extract predicted value from the posterior
  # they were generated during fitting they are in LONG format
  dv_pred <- rstan::extract(object$stanfit)$predicted

  # compute residuals
  e <- -1 * sweep(dv_pred, 2, c(object$data$dv))

  # compute variances
  var_dv_pred <- apply(dv_pred, 1, stats::var)
  var_e <- apply(e, 1, stats::var)

  # compute R2
  r2s <- var_dv_pred / (var_dv_pred + var_e)

  # returning
  if (!summary) {
    r2s
  }
  TriDimRegression::variable_summary("R2", as.matrix(r2s), probs)
}

#' @export
#' @keywords internal
R2 <- function(object, ...) { UseMethod("R2") }
