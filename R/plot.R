#' Posterior interval plots for key parameters. Uses bayesplot::mcmc_intervals.
#'
#' @param x A [tridim_transformation][tridim_transformation-class()] object
#' @param ... Extra parameters to be passed to [bayesplot::mcmc_intervals()]
#' @return A ggplot object produced by [bayesplot::mcmc_intervals()]
#'
#' @export
#'
#' @examples
#' \donttest{
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'                            data = NakayaData,
#'                            transformation = 'euclidean')
#' plot(euc2)
#' }
plot.tridim_transformation <- function(x, ...){
  if (x$dimN == 2){
    # bidimensional regression
    coef_names <- c("scale", "shear", "rotation", "tilt", "translation")
  }
  else {
    coef_names <- c("rotation", "scale", "translation", "shearX", "shearY", "shearZ")
  }

  bayesplot::mcmc_intervals(as.matrix(x$stanfit, pars = coef_names))
}

# setMethod("plot", "tridim_transformation", plot.tridim_transformation)
