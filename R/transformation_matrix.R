#' 2D transformation matrix, based on transformation type
#'
#' @param params a single row data.frame with parameters. Vector parameters must be name
#' with .X dimension suffix. E.g., scale.1 and scale.2.
#' @param transformation string, \code{"euclidean"}, \code{"affine"}, or \code{"projective"}
#'
#' @return matrix 3x3
#' @export
#'
#' @examples
#' bi_transformation_matrix(
#'   data.frame(rotation=0,
#'              scale.1=1, scale.2=2,
#'              translation.1=0, translation.2=-5),
#'   "euclidean")
bi_transformation_matrix <- function(params, transformation){
  transform_matrix <- TriDimRegression::bi_scale_matrix(params$scale.1, params$scale.2)
  if (transformation != "euclidean"){
    transform_matrix <- transform_matrix %*% TriDimRegression::bi_shear_matrix(0, params$shear)
  }
  transform_matrix <- transform_matrix %*% TriDimRegression::bi_rotation_matrix(params$rotation)
  if (transformation == "projective"){
    transform_matrix <- transform_matrix %*% TriDimRegression::bi_tilt_matrix(params$tilt.1, params$tilt.2)
  }
  transform_matrix %*% TriDimRegression::bi_translation_matrix(params$translation.1, params$translation.2)
}


#' 3D transformation matrix, based on transformation type
#'
#' @param params a single row data.frame with parameters. Vector parameters must be name
#' with .X dimension suffix. E.g., scale.1 and scale.2.
#' @param transformation string, \code{"euclidean_x"}, \code{"euclidean_y"},
#' \code{"euclidean_z"}, \code{"euclidean"}, \code{"affine"}, or \code{"projective"}
#'
#' @return matrix 4x4
#' @export
#'
#' @examples
#' tri_transformation_matrix(data.frame(rotation=0,
#'                                      scale.1=1, scale.2=2, scale.3=1,
#'                                      translation.1=0, translation.2=-5, translation.3=-4),
#'                          "euclidean_z")
tri_transformation_matrix <- function(params, transformation){
  transform_matrix <- TriDimRegression::tri_scale_matrix(params$scale.1, params$scale.2, params$scale.3)
  if (transformation %in% c("affine", "projective")){
    transform_matrix <- transform_matrix %*% TriDimRegression::tri_shear_x_matrix(params$shearX.1, params$shearX.2)
  }
  if (transformation == "projective") {
    transform_matrix <- transform_matrix %*%
      TriDimRegression::tri_shear_y_matrix(params$shearY.1, params$shearY.2) %*%
      TriDimRegression::tri_shear_z_matrix(params$shearZ.1, params$shearZ.2)
  }
  if (!transformation %in% c("euclidean_y", "euclidean_z")){
    if (transformation == "euclidean_x") {
      transform_matrix <- transform_matrix %*% TriDimRegression::tri_rotation_x_matrix(params$rotation);
    }
    else {
      transform_matrix <- transform_matrix %*% TriDimRegression::tri_rotation_x_matrix(params$rotation.1);
    }
  }

  if (!transformation %in% c("euclidean_x", "euclidean_z")){
    if (transformation == "euclidean_y") {
      transform_matrix <- transform_matrix %*% TriDimRegression::tri_rotation_y_matrix(params$rotation);
    }
    else {
      transform_matrix <- transform_matrix %*% TriDimRegression::tri_rotation_y_matrix(params$rotation.2);
    }
  }

  if (!transformation %in% c("euclidean_x", "euclidean_y")){
    if (transformation == "euclidean_z") {
      transform_matrix <- transform_matrix %*% TriDimRegression::tri_rotation_z_matrix(params$rotation);
    }
    else {
      transform_matrix <- transform_matrix %*% TriDimRegression::tri_rotation_z_matrix(params$rotation.3);
    }
  }

  transform_matrix %*% TriDimRegression::tri_translation_matrix(params$translation.1, params$translation.2, params$translation.3);
}

#' Transformation matrix, 2D or 3D dependening on data and transformation type
#'
#' @name transformation_matrix
#' @param object [tridim_transform][tridim_transform-class()] object
#' @param summary Whether summary statistics should be returned instead of
#' raw sample values. Defaults to \code{TRUE}
#'
#' @return matrix 3x3  for 2D transformation or matrix 4x4 for 3D transformation
#' @export
#'
#' @examples
#' \dontrun{
#' euc2 <- fit_geometric_transformation(depV1+depV2~indepV1+indepV2,
#'   NakayaData, transformation = 'euclidean')
#' transformation_matrix(euc2)
#' }
NULL

#' @export
transformation_matrix.tridim_transformation <- function(object, summary=TRUE){
  # getting parameter samples
  param_samples <-
    rstan::extract(object$stanfit,
                   pars=c(paste0("a", object$dimN), paste0("b", 1:get_beta_n(object$dimN, object$transform)))) %>%
    data.frame()

  # setting up a list of matrix functions
  if (object$dimN == 2 ){
    mfun <- c("translation" = m2_translation,
              "euclidean" =   m2_euclidean,
              "affine" =      m2_affine,
              "projective" =  m2_projective)
  }
  else if (object$dimN == 3){
    mfun <- c("translation" = m3_translation,
              "euclidean_x" = m3_euclidean_x,
              "euclidean_y" = m3_euclidean_y,
              "euclidean_z" = m3_euclidean_z,
              "affine" =      m3_affine,
              "projective" =  m3_projective)

  }
  else stop("Number of dimensions must be either 2 or 3")
  if (!object$transformation %in% names(mfun)) stop(sprintf("Unknown transformation %s", object$transformation))

  # generate matrices for each sample
  matrices <- purrr::map(1:nrow(param_samples),
                         ~mfun[object$transformation](param_samples[., ]))

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
