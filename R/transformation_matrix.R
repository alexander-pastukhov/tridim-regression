#' Transformation matrix, 2D or 3D depending on data and transformation type
#'
#' @name transformation_matrix
#' @param object [tridim_transformation][tridim_transformation-class()] object
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#'
#' @return matrix 3x3  for 2D transformation or matrix 4x4 for 3D transformation
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_transformation(depV1+depV2~indepV1+indepV2,
#'                            data = NakayaData,
#'                            transformation = 'euclidean')
#' transformation_matrix(euc2)
#' }
NULL

#' @export
transformation_matrix.tridim_transformation <- function(object, summary=TRUE){
  # getting parameter samples
  param_samples <- rstan::extract(object$stanfit, pars=c("a", "b"))

  # setting up a list of matrix functions
  if (object$dimN == 2 ){
    mfun <- list("translation" = m2_translation,
                 "euclidean" =   m2_euclidean,
                 "affine" =      m2_affine,
                 "projective" =  m2_projective)
  }
  else if (object$dimN == 3){
    mfun <- list("translation" = m3_translation,
                 "euclidean_x" = m3_euclidean_x,
                 "euclidean_y" = m3_euclidean_y,
                 "euclidean_z" = m3_euclidean_z,
                 "affine" =      m3_affine,
                 "projective" =  m3_projective)

  }
  else stop("Number of dimensions must be either 2 or 3")
  if (!object$transformation %in% names(mfun)) stop(sprintf("Unknown transformation %s", object$transformation))

  testf <<- mfun[object$transformation]

  # generate matrices for each sample
  matrices <- purrr::map(1:nrow(param_samples$a),
                         ~mfun[[object$transformation]](param_samples$a[., ],
                                                        param_samples$b[., ]))

  if (!summary) {
    return(matrices)
  }

  # mean transform
  matrix(unlist(matrices), byrow=TRUE, nrow=length(matrices) ) %>%
    colMeans() %>%
    matrix(ncol=object$dimN+1, byrow = TRUE)
}

#' @export
transformation_matrix <- function(object, summary) { UseMethod("transformation_matrix") }
