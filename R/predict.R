#' Computes posterior samples for the posterior predictive distribution.
#'
#' Predicted values based on the bi/tridimensionsal regressional model object.
#'
#' @param object An object of class "tridim_transform"
#' @param newdata An optional two column data frame with independent variables.
#' If omitted, the fitted values are used.
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to NULL (no CI).
#'
#' @return If summary=FALSE, a numeric matrix [iterationsN, observationsN, variablesN].
#' If summary=TRUE, a data.frame with columns "dv{index}" with mean for each dependent
#' variable plus optional quantiles columns with names "dv{index}_{quantile}".
#' @export
#'
#' @seealso \code{\link{fit_geometric_transformation}}
#' @examples
#' \dontrun{
#' euc2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2, NakayaData, transformation = 'euclidean')
#'
#' # prediction summary
#' predictions <- predict(euc2)
#'
#' # full posterior prediction samples
#' prediction_samples <- predict(euc2, summary=FALSE)
#' }
predict.tridim_transform <-  function(object, newdata=NULL, summary=TRUE, probs=NULL) {
  if (is.null(newdata)) {
    # we can reuse already computed predictions
    prediction_samples <- rstan::extract(object$stanfit)
    prediction_samples <- rstan::extract(object$stanfit)$predicted[, , 1:object$dimN]
  }
  else {
    # let's try getting the data
    iv <- cbind(as.matrix(Formula::model.part(Formula::Formula(euc2$formula), data = newdata, rhs = 1)), 1)

    # getting the transformation matrices
    transform <- TriDimRegression::transformation_matrix(object, summary=FALSE)

    # transforming independent variables to obtain predictions
    prediction_samples <-
      purrr::map(transform, ~(iv %*% .) %>% t()) %>%
      simplify2array(.) %>%
      aperm(.) %>%
      .[, , 1:object$dimN] # last dimensions is trivially 1
  }

  if (!summary) {
    # raw samples
    return(prediction_samples)
  }

  # summary
  purrr::map(1:object$dimN,
             ~TriDimRegression::variable_summary(colnames(object$data$dv)[.], prediction_samples[, , .], probs=probs)) %>%
    dplyr::bind_cols()
}
