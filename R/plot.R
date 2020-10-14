#' Posterior interval plots for key parameters. Uses bayesplot::mcmc_intervals.
#'
#' @param x A [tridim_transform][tridim_transform-class()] object
#' @param ... Extra parameters to be passed to [bayesplot::mcmc_intervals()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2,
#'                                         data = NakayaData,
#'                                         transformation = 'euclidean')
#' plot(euc2)
#' }
plot.tridim_transform <- function(x, ...){
  if (x$dimN == 2){
    # bidimensional regression
    coef_names <- c("scale", "shear", "rotation", "tilt", "translation")
  }
  else {
    coef_names <- c("rotation", "scale", "translation", "shearX", "shearY", "shearZ")
  }

  bayesplot::mcmc_intervals(as.matrix(x$stanfit, pars = coef_names))
}

# setMethod("plot", "tridim_transform", plot.tridim_transform)
