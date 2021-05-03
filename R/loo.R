#' Computes an efficient approximate leave-one-out
#' cross-validation via loo library. It can be used
#' for a model comparison via loo::loo_compare() function.
#'
#' @param x A [tridim_transformation][tridim_transformation-class()] object
#' @param ... unused
#'
#' @return A named list, see [loo::loo()] for details.
#' @export
#'
#' @examples
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'   NakayaData, transformation = 'euclidean')
#' aff2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'   NakayaData, transformation = 'affine')
#' loo::loo_compare(loo(euc2), loo(aff2))
loo.tridim_transformation <- function(x, ...) {
  rstan::loo(x$stanfit)
}
