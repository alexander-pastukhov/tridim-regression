#' 2D Translation Matrix
#'
#' @param a numeric, 2: translation
#' @param b numeric, ignored
#'
#' @return matrix 3x3
#' @export
#' @keywords internal
#'
#' @examples
#' m2_translation(c(2, 3))
m2_translation <- function(a, b=NULL){
  stopifnot(all(is.finite(a)), length(a)==2)
  matrix(c(1,    0,    0,
           0,    1,    0,
           a[1], a[2], 1),
         ncol=3, byrow=TRUE)
}

#' 2D Euclidean
#'
#' @param a numeric, 2: translation
#' @param b numeric, 2: all other coefficients
#'
#' @return matrix 3x3
#' @keywords internal
#' @export
#' @examples
#' m2_euclidean(c(2, 3), c(1, 0.5))
m2_euclidean <- function(a, b){
  stopifnot(all(is.finite(a)), length(a)==2, all(is.finite(b)), length(b)==2)
  matrix(c(b[1],-b[2], 0,
           b[2], b[1], 0,
           a[1], a[2], 1),
         ncol=3, byrow=TRUE)
}

#' 2D Affine
#'
#' @param a numeric, 2: translation
#' @param b numeric, 4: all other coefficients
#'
#' @return matrix 3x3
#' @keywords internal
#' @export
#' @examples
#' m2_affine(c(2, 3), c(1, 0.5, 1, 5))
m2_affine <- function(a, b){
  stopifnot(all(is.finite(a)), length(a)==2, all(is.finite(b)), length(b)==4)
  matrix(c(b[1], b[3], 0,
           b[2], b[4], 0,
           a[1], a[2], 1),
         ncol=3, byrow=TRUE)
}


#' 2D Projective
#'
#' @param a numeric, 2: translation
#' @param b numeric, 6: all other coefficients
#'
#' @return matrix 3x3
#' @keywords internal
#' @export
#' @examples
#' m2_projective(c(2, 3), c(1, 0.5, 1, 5, 2, 4))
m2_projective <- function(a, b){
  stopifnot(all(is.finite(a)), length(a)==2, all(is.finite(b)), length(b)==6)
  matrix(c(b[1], b[3], b[5],
           b[2], b[4], b[6],
           a[1], a[2], 1),
         ncol=3, byrow=TRUE)
}
