#' Prints out tridim_transform object
#'
#' @param x A [tridim_transform][tridim_transform-class()] object
#' @param ... Unused
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

print.tridim_transform <- function(x, ...){
  cat('Call: ')
  cat(deparse(x$formula))
  cat('\n')

  cat(glue::glue('Data dimensions: {x$dimN}\n\n'))
  cat(glue::glue('Transformation: {x$transformation}\n\n'))
  cat(glue::glue('\n\nCoefficients:\n\n'))
  printCoefmat(tidyr::pivot_wider(coef.tridim_transform(x, summary=TRUE, probs = NULL),
                                  names_from="Coef", values_from="Mean"))
}

# setMethod("print", "tridim_transform", print.tridim_transform)
