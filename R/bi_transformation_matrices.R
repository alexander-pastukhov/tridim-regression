#' 2D Translation Matrix
#'
#' @param params numeric, coefficients a1 and a2
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
#' @param params numeric, coefficients a1, a2, b1, b2
#'
#' @return matrix 3x3
#' @export
#' @examples
#' m2_euclidean(c(2, 3, 1, 0.5))
m2_euclidean <- function(p){
  matrix(c(p[2 + 1],-p[2 + 2], 0,
           p[2 + 2], p[2 + 1], 0,
           p[1],     p[2],     1),
         ncol=3, byrow=TRUE)
}

#' 2D Affine
#'
#' @param params numeric, coefficients a1, a2, b1...b4
#'
#' @return matrix 3x3
#' @export
#' @examples
#' m2_affine(c(2, 3, 1, 0.5, 1, 5))
m2_affine <- function(p){
  matrix(c(p[2 + 1], p[2 + 3], 0,
           p[2 + 2], p[2 + 4], 0,
           p[1],     p[2],     1),
         ncol=3, byrow=TRUE)
}


#' 2D Projective
#'
#' @param params numeric, coefficients a1, a2, b1...b6
#'
#' @return matrix 3x3
#' @export
#' @examples
#' m2_projective(c(2, 3, 1, 0.5, 1, 5, 2, 4))
m2_projective <- function(p){
  matrix(c(p[2 + 1], p[2 + 3], p[2 + 5],
           p[2 + 2], p[2 + 4], p[2 + 6],
           p[1],     p[2],     1),
         ncol=3, byrow=TRUE)
}
