#' Summary for a tridim_transform object
#'
#' @param object A tridim_transform object
#'
#' @export
#'
#' @examples
#' euc2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2,
#'                                      data = NakayaData,
#'                                      transformation = 'euclidean')
#' summary(euc2)

summary.tridim_transform <- function(object){
  cat('Call: ')
  cat(deparse(object$formula))
  cat('\n')

  cat(glue::glue('Data dimensions: {object$dimN}\n\n'))
  cat(glue::glue('Transformation: {object$transformation}\n\n'))
  theR2 <- round(TriDimRegression::R2(object, adjust = FALSE), 3)
  cat(glue::glue('R2: {theR2[1,1]} [{theR2[1,2]}..{theR2[1,3]}]\n\n'))
  theAdjR2 <- round(TriDimRegression::R2(object, adjust = TRUE), 3)
  cat(glue::glue('Adjusted R2: {theAdjR2[1,1]} [{theAdjR2[1,2]}..{theAdjR2[1,3]}]\n\n'))

  cat(glue::glue('\n\nCoefficients:\n\n'))
  print(TriDimRegression::coef(object, summary=TRUE), row.names = FALSE)
}
