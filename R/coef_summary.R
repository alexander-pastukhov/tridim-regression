#' Computes mean and optional quantiles for a coefficient.
#'
#' @param coef_name String, name of the coefficient.
#' @param coef_matrix Numeric matrix [samplesN, 2] or a
#' numeric vector [samplesN].
#' @param probs A numeric vector of quantiles to compute.
#'
#' @return data.frame with columns "Coef", "Mean", and a column
#' for each quantile.
#' @keywords internal
#' @export
coef_summary <- function(coef_name, coef_matrix, probs){
  # adding indexing to names, if coefficients is a vector
  if (length(dim(coef_matrix)) > 1 && ncol(coef_matrix) > 1){
    coef_name <- glue::glue("{coef_name}[{1:ncol(coef_matrix)}]")
  }

  # computing stats
  coef_stats <-
    cbind(
      data.frame("Coef" = coef_name,
                 "Mean" = apply(as.matrix(coef_matrix), MARGIN=2, FUN=mean)),
      data.frame(t(apply(as.matrix(coef_matrix), MARGIN=2, FUN=quantile, probs=probs)))
    )
  names(coef_stats) <- c("Coef", "Mean", glue::glue("{probs*100}%"))
  coef_stats
}
