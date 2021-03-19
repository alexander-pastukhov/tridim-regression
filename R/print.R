#' Prints out tridim_transformation object
#'
#' @param x A [tridim_transformation][tridim_transformation-class()] object
#' @param ... Unused
#'
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'                            data = NakayaData,
#'                            transformation = 'euclidean')
#' euc2
#' }

print.tridim_transformation <- function(x, ...){
  cat('Call: ')
  cat(deparse(x$formula))
  cat('\n')

  cat(glue::glue('Data dimensions: {x$dimN}\n\n'))
  cat(glue::glue('Transformation: {x$transformation}\n\n'))
  cat(glue::glue('\n\nCoefficients:\n\n'))
  printCoefmat(tidyr::pivot_wider(coef.tridim_transformation(x, summary=TRUE, probs = NULL),
                                  names_from="Coef", values_from="Mean"))
}

# setMethod("print", "tridim_transformation", print.tridim_transformation)
