#' @export
loo <- function(object, ...) { UseMethod("loo") }

#' Computes an efficient approximate leave-one-out
#' cross-validation via loo library. It can be used
#' for a model comparison via loo::loo_compare() function.
#'
#' @param object A tridim_transform object
#'
#' @return A named list, see loo::loo() for details.
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2, NakayaData, transformation = 'euclidean')
#' aff2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2, NakayaData, transformation = 'affine')
#' loo::loo_compare(loo(euc2), loo(aff2))
#' }
loo.tridim_transform <- function(object) {
  log_lik <- loo::extract_log_lik(object$stanfit, "log_lik", merge_chains = FALSE)

  if (length(dimnames(as.array(object$stanfit))$chains) == 1){
    # single chaing, no relative_eff
    return( loo::loo(log_lik, cores = future::availableCores()) )
  }


  # we have more than one chain, can compute relative_eff
  r_eff <- loo::relative_eff(exp(log_lik), cores=future::availableCores())
  loo::loo(log_lik, r_eff = r_eff, cores = future::availableCores())
}
