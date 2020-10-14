#' Posterior distributions for transformation coefficients
#' in full or summarized form.
#'
#' @param object An object of class [tridim_transform][tridim_transform-class()].
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#' @param ... Unused
#'
#' @return If summary=FALSE, a list with matrix iterationsN x dimensionsN for
#' each variable.
#' If summary=TRUE, a data.frame with columns "dv{index}" with mean for each dependent
#' variable plus optional quantiles columns with names "dv{index}_{quantile}".

#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2,
#'                                         data = NakayaData,
#'                                         transformation = 'euclidean')
#'
#' # full posterior distribution
#' transform_posterior <- coef(euc2, summary=FALSE)
#'
#' # coefficients' summary with 89% CI
#' coef(euc2)
#' }
coef.tridim_transform <- function(object, summary=TRUE,  probs=c(0.055, 0.945), ...){
  if (object$dimN == 2){
    # bidimensional regression
    coef_names <- c("scale", "shear", "rotation", "tilt", "translation")
  }
  else {
    coef_names <- c("rotation", "scale", "translation", "shearX", "shearY", "shearZ")
  }

  coef_samples <- rstan::extract(object$stanfit, pars=coef_names)
  if (summary==FALSE){
    return(coef_samples)
  }

  purrr::map_df(names(coef_samples), ~coef_summary(., coef_samples[[.]], probs))
}
