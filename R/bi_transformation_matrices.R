#' 2D Translation Matrix
#'
#' @param a numeric, coefficients a1 and a2
#'
#' @return matrix 3x3
#' @export
#'
#' @examples
#' m2_translation(c(2, 3))
m2_translation <- function(a){
  matrix(c(1,    0,    0,
           0,    1,    0,
           a[1], a[2], 1),
         ncol=3, byrow=TRUE)
}

#' 2D Euclidean
#'
#' @param a numeric, coefficients a1, a2, b1, b2
#'
#' @return matrix 3x3
#' @export
#' @examples
#' m2_euclidean(c(2, 3, 1, 0.5))
m2_euclidean <- function(a){
  b <- a[c(-1, -2)]
  matrix(c(b[1],-b[2], 0,
           b[2], b[1], 0,
           a[1], a[2], 1),
         ncol=3, byrow=TRUE)
}

#' 2D Affine
#'
#' @param a numeric, coefficients a1, a2, b1...b4
#'
#' @return matrix 3x3
#' @export
#' @examples
#' m2_affine(c(2, 3, 1, 0.5, 1, 5))
m2_affine <- function(a){
  b <- a[c(-1, -2)]
  matrix(c(b[1], b[3], 0,
           b[2], b[4], 0,
           a[1], a[2], 1),
         ncol=3, byrow=TRUE)
}


#' 2D Projective
#'
#' @param a numeric, coefficients a1, a2, b1...b6
#'
#' @return matrix 3x3
#' @export
#' @examples
#' m2_projective(c(2, 3, 1, 0.5, 1, 5, 2, 4))
m2_projective <- function(a){
  b <- a[c(-1, -2)]
  matrix(c(b[1], b[3], b[5],
           b[2], b[4], b[6],
           a[1], a[2], 1),
         ncol=3, byrow=TRUE)
}
