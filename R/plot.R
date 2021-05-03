#' Posterior interval plots for key parameters. Uses bayesplot::mcmc_intervals.
#'
#' @param x A [tridim_transformation][tridim_transformation-class()] object
#' @param convert_euclidean Whether to convert matrix coefficients to scale(phi) and rotation(theta). Defaults to \code{FALSE}.
#' @param ... Extra parameters to be passed to [bayesplot::mcmc_intervals()]
#' @return A ggplot object produced by [bayesplot::mcmc_intervals()]
#'
#' @export
#'
#' @examples
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'                            data = NakayaData,
#'                            transformation = 'euclidean')
#' plot(euc2)
#'
#' # same but for converted coefficients
#' plot(euc2, convert_euclidean=TRUE)
plot.tridim_transformation <- function(x, convert_euclidean=FALSE, ...){
  bayesplot::mcmc_intervals(coef(x, summary=FALSE, convert_euclidean=convert_euclidean))
}

# setMethod("plot", "tridim_transformation", plot.tridim_transformation)
