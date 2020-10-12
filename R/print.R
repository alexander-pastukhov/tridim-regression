#' Prints out tridim_transform object
#'
#' @param object A tridim_transform object
#'
#' @export
#'
#' @examples
#' euc2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2,
#'                                      data = NakayaData,
#'                                      transformation = 'euclidean')
#' euc2

print.tridim_transform <- function(object){
  cat('Call: ')
  cat(deparse(object$formula))
  cat('\n')

  cat(glue::glue('Data dimensions: {object$dimN}\n\n'))
  cat(glue::glue('Transformation: {object$transformation}\n\n'))
  cat(glue::glue('\n\nCoefficients:\n\n'))
  printCoefmat(tidyr::pivot_wider(TriDimRegression::coef(object, summary=TRUE, probs = NULL),
                                  names_from="Coef", values_from="Mean"))
}
