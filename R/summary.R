#' Summary for a tridim_transformation object
#'
#' @param object A [tridim_transformation][tridim_transformation-class()] object
#' @param ... Unused
#'
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'                            data = NakayaData,
#'                            transformation = 'euclidean')
#' summary(euc2)
#' }

summary.tridim_transformation <- function(object, ...){
  cat('Call: ')
  cat(deparse(object$formula))
  cat('\n')

  cat(glue::glue('Data dimensions: {object$dimN}\n\n'))
  cat(glue::glue('Transformation: {object$transformation}\n\n'))
  theR2 <- round(TriDimRegression::R2(object, adjust = FALSE), 3)
  cat(glue::glue('Bayesian R2: {theR2[1,1]} [{theR2[1,2]}..{theR2[1,3]}]\n\n'))

  cat(glue::glue('\n\nCoefficients:\n\n'))
  print(coef.tridim_transformation(object, summary=TRUE), row.names = FALSE)
}

# setMethod("summary", "tridim_transformation", summary.tridim_transformation)
