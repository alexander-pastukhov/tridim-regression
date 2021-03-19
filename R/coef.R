#' Posterior distributions for transformation coefficients
#' in full or summarized form.
#'
#' @param object An object of class [tridim_transformation][tridim_transformation-class()].
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#' @param convert_euclidean Whether to convert matrix coefficients to scale(phi) and rotation(theta). Defaults to \code{FALSE}.
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
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'                            data = NakayaData,
#'                            transformation = 'euclidean')
#'
#' # full posterior distribution
#' transform_posterior <- coef(euc2, summary=FALSE)
#'
#' # coefficients' summary with 89% CI
#' coef(euc2)
#'
#' # scale and rotation coefficients
#' coef(euc2, convert_euclidean=TRUE)
#' }
coef.tridim_transformation <- function(object,
                                       summary=TRUE,
                                       probs=c(0.055, 0.945),
                                       convert_euclidean=FALSE, ...){

  # coefficient names for the columns
  As <- 1:object$dimN
  names(As) <- paste0("a", 1:object$dimN)
  if (object$data$betaN > 0){
    Bs <- 1:object$data$betaN
    names(Bs) <- paste0("b", 1:object$data$betaN)
    new_names <- list(As, Bs)
  }
  else {
    new_names <- list(As)
  }


  # getting samples and converting to a single data.frame
  coef_samples <-
    purrr::map2(rstan::extract(object$stanfit, pars=c("a", "b")),
                new_names,
                ~data.frame(.x) %>% dplyr::rename(.y)) %>%
    dplyr::bind_cols()

  # A special case of Eucledean transformation
  if (startsWith(object$transformation, "euclidean") && convert_euclidean){
    coef_samples <-
      coef_samples %>%
      dplyr::mutate(scale = sqrt(b1^2 + b2^2),
                    rotation = atan2(b2, b1)) %>%
      dplyr::select(!c(b1, b2))
  }

  if (summary==FALSE){
    return(coef_samples)
  }

  purrr::map_df(names(coef_samples), ~coef_summary(., coef_samples[[.]], probs))
}
