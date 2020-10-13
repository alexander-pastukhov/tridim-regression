#' Prints out tridim_transform object
#'
#' @param object A tridim_transform object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2,
#'                                      data = NakayaData,
#'                                      transformation = 'euclidean')
#' euc2
#' }

print.tridim_transform <- function(object){
  cat('Call: ')
  cat(deparse(object$formula))
  cat('\n')

  cat(glue::glue('Data dimensions: {object$dimN}\n\n'))
  cat(glue::glue('Transformation: {object$transformation}\n\n'))
  cat(glue::glue('\n\nCoefficients:\n\n'))
  printCoefmat(tidyr::pivot_wider(coef.tridim_transform(object, summary=TRUE, probs = NULL),
                                  names_from="Coef", values_from="Mean"))
}

# setMethod("print", "tridim_transform", print.tridim_transform)
