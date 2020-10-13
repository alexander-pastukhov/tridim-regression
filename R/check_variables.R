#' Checks validity of variables' matrix
#'
#' Checks whether the matrix is numeric,
#' has expected number of columns (\code{ncol}),
#' and has no missing/infinite data.
#'
#'
#' @param var Matrix N x ncol
#' @param var_label Variable label for error messages
#' @param ncol Expected number of columns
#' @return NULL
#' @keywords internal
#' @export
#'
#' @examples
#' check_variables(matrix(c(1, 2, 3, 4), ncol=2), "test matrix")
check_variables <- function(var, var_label){
  if (!is.numeric(var)) stop(sprintf("%s is not a numeric", var_label))
  if (sum(!is.finite(var)) > 0) stop(sprintf("%s has non-finite elements", var_label))
}
