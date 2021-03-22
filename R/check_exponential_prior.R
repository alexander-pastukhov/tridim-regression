#' Checks for validity of values for use as exponential distribution parameters.
#'
#' Should a single non-negative numeric value.
#' Stops execution with an error.
#'
#' @param values Parameter for exponential distribution.
#' @param parameter Name of the parameter for which the prior is defined.
#'
#' @return NULL
#' @export
#' @keywords internal
#'
#' @examples
#' check_exponential_prior(1, "sigma")
check_exponential_prior <- function(values, parameter){
  if (!is.numeric(values)) stop(sprintf("prior parameters for %s is not a numeric", parameter))
  if (length(values) != 1) stop(sprintf("prior parameters for %s should be a single value", parameter))
  if (values <= 0) stop(sprintf("prior for %s should be strictly positive", parameter))
}

