#' Computes posterior samples for the posterior predictive distribution.
#'
#' Predicted values based on the bi/tridimensionsal regressional model object.
#'
#' @param object An object of class [tridim_transformation][tridim_transformation-class()]
#' @param newdata An optional two column data frame with independent variables.
#' If omitted, the fitted values are used.
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#' @param probs The percentiles used to compute summary, defaults to NULL (no CI).
#' @param ... Unused
#'
#' @return If summary=FALSE, a numeric matrix iterationsN x observationsN x variablesN.
#' If summary=TRUE, a data.frame with columns "dv{index}" with mean for each dependent
#' variable plus optional quantiles columns with names "dv{index}_{quantile}".
#' @export
#'
#' @seealso \code{\link{fit_transformation}}
#' @examples
#' \dontrun{
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'   NakayaData, transformation = 'euclidean')
#'
#' # prediction summary
#' predictions <- predict(euc2)
#'
#' # full posterior prediction samples
#' predictions <- predict(euc2, summary=FALSE)
#' }
predict.tridim_transformation <-  function(object, newdata=NULL, summary=TRUE, probs=NULL, ...) {
  if (is.null(newdata)) {
    # we can reuse already computed predictions
    predictions <- rstan::extract(object$stanfit, pars="predicted")$predicted
    predictions <- array(predictions, c(nrow(predictions), object$data$rowsN, object$data$varsN))
  }
  else {
    # let's try getting the data
    iv <- cbind(as.matrix(Formula::model.part(Formula::Formula(object$formula), data = newdata, rhs = 1)), 1)

    # getting the transformation matrices
    transform <- TriDimRegression::transformation_matrix(object, summary=FALSE)

    # transforming independent variables to obtain predictions
    predictions <-
      purrr::map(transform, ~(iv %*% .) %>% t()) %>%
      simplify2array() %>%
      aperm()


    # last dimensions is 1 due to homogeneous coordinates
    predictions <- predictions[, , 1:object$dimN]
  }

  if (!summary) {
    # raw samples
    return(predictions)
  }

  # summary
  purrr::map(1:object$dimN,
             ~TriDimRegression::variable_summary(colnames(object$data$dv)[.], predictions[, , .], probs=probs)) %>%
    dplyr::bind_cols()
}

# setMethod("predict", "tridim_transformation", predict.tridim_transformation)

