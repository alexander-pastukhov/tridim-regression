#' Computes widely applicable information criterion
#' (WAIC).
#'
#' @description Computes widely applicable information criterion
#' via [loo][loo-package] library. It can be used for a model comparison via
#' [loo::loo_compare()][loo::loo_compare] function.
#'
#' @param object A [tridim_transformation[[tridim_transformation-class()] object
#'
#' @return A named list, see [loo::waic()] for details.
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'   NakayaData, transformation = 'euclidean')
#' aff2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'   NakayaData, transformation = 'affine')
#' loo::loo_compare(waic(euc2), waic(aff2))
#' }
waic.tridim_transformation <- function(object) {
  log_lik <- loo::extract_log_lik(object$stanfit, "log_lik", merge_chains = FALSE)
  loo::waic(log_lik)
}

