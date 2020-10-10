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
  # sanity checks
  stopifnot(is.character(coef_name), !is.na(coef_name))
  stopifnot(is.numeric(probs) || is.null(probs))
  stopifnot(sum(is.finite(coef_matrix)) == length(coef_matrix))

  # adding indexing to names, if coefficients is a vector
  if (length(dim(coef_matrix)) > 1 && ncol(coef_matrix) > 1){
    coef_name <- glue::glue("{coef_name}[{1:ncol(coef_matrix)}]")
  }

  # computing stats
  coef_stats <- data.frame("Coef" = coef_name,
                           "Mean" = apply(as.matrix(coef_matrix), MARGIN=2, FUN=mean))

  if (length(probs) > 0) {
    if (length(probs) > 1) {
      CIs <- data.frame(t(apply(as.matrix(coef_matrix), MARGIN=2, FUN=quantile, probs=probs)))
    }
    else {
      CIs <- data.frame(apply(as.matrix(coef_matrix), MARGIN=2, FUN=quantile, probs=probs))
    }
    names(CIs) <- glue::glue("{probs*100}")
    coef_stats <- cbind(coef_stats, CIs)
  }

  coef_stats
}

