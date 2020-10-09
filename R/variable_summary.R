#' Computes mean and optional probabilities for a given
#' variable.
#'
#' @param var_name String, name of the dependent variable
#' @param var_matrix Numeric matrix [samplesN, observationsN].
#' @param probs A numeric vector of quantiles to compute.
#'
#' @return data.frame with "{var_name}" column for the mean and
#' "dv{var_name}_{prob}" columns for each probability.
#' @keywords internal
#' @export
variable_summary <- function(var_name, var_matrix, probs){
  var_summary <- list()
  var_summary[[glue::glue("{var_name}")]] <- apply(var_matrix, MARGIN=2, FUN=mean)
  if (is.null(probs)) return(as.data.frame(var_summary))

  computed_probs <- purrr::map(probs, ~apply(var_matrix, MARGIN=2, FUN=quantile, probs=.))
  names(computed_probs) <- glue::glue("{var_name}_{probs*100}")
  as.data.frame(c(var_summary, computed_probs))
}
