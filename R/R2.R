#' Computes R-squared or adjusted R-squared
#'
#' @name R2
#'
#' @param object An object of class "tridim_transform"
#' @param adjust Whether R-squared should be adjusted, defaults to \code{TRUE}
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to 89% credible interval.
#'
#' @return vector of values or a data.frame with summary
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2,
#'   NakayaData, transformation = 'euclidean')
#' R2(euc2)
#' }
R2.tridim_transform <- function(object, adjust=TRUE, summary=TRUE, probs=c(0.055, 0.945)){
  # posterior predictions
  predictions <- predict.tridim_transform(object, summary=FALSE)

  # computing unadjusted R2
  total_variance <- sum((object$data$dv-matrix(rep(colMeans(object$data$dv), nrow(object$data$dv)), ncol=object$dimN, byrow=TRUE))^2)
  r2s <- purrr::map_dbl(1:nrow(predictions),
                        ~(1 - sum((predictions[., , ] - object$data$dv)^2) / total_variance))

  # adjusting for number of parameters
  if (adjust == TRUE) {
    if (object$dimN == 2) {
      df <- switch(object$transformation,
                   "euclidean" = 5,
                   "affine" = 6,
                   "projective" = 8)
    }
    else{
      df <- switch(object$transformation,
                   "euclidean_x" = 7,
                   "euclidean_y" = 7,
                   "euclidean_z" = 7,
                   "euclidean" = 9,
                   "affine" = 11,
                   "projective" = 15)
    }

    N <- nrow(object$data$dv)
    r2s <- 1 - (((N-1)/(N-df-1)) * ((N-2)/(N-df-2)) * ((N+1)/N))*(1-r2s)
  }

  # returning
  if (!summary) {
    r2s
  }
  TriDimRegression::variable_summary("R2", as.matrix(r2s), probs)
}

#' @export
R2 <- function(x, ...) { UseMethod("R2") }
