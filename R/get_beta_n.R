#' Returns number of free matrix parameters in addition to translation
#'
#' @param dimN integer, either 2 or 3
#' @param transformation string, for 2D \code{"translation"}, \code{"euclidean"}, \code{"affine"}, \code{"projective"}. for 3D \code{"translation"}, \code{"euclidean_x"}, \code{"euclidean_y"}, \code{"euclidean_z"}, \code{"affine"}, \code{"projective"}.
#'
#' @return integer
#' @export
#'
#' @examples
#' get_beta_n(2, "euclidean") # should return 2
#' get_beta_n(3, "affine") # should return 9
get_beta_n <- function(dimN, transformation) {
  # setup constants and check for dimensionality
  if (dimN == 2) {
    param_n <- c("translation"=0, "euclidean"=2, "affine"=4, "projective"=6)
  }
  else if (dimN == 3){
    param_n <- c("translation"=0, "affine"=9, "projective"=12, "euclidean_x"= 2, "euclidean_y" = 2, "euclidean_z" = 2)
  }
  else {
    stop("Number of dimensions must be either 2 or 3")
  }

  # check whether the name is valid
  if (!transformation %in% names(param_n)) stop(sprintf("Unknown transformation %s", transformation))

  param_n[transformation]
}
