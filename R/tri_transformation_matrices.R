#' 3D Translation Matrix
#'
#' @param a numeric, translation
#' @param b numeric, all other coefficients
#'
#' @return matrix 4x4
#' @keywords internal
#' @export
#'
#' @examples
#' m3_translation(c(2, 3, 1))
m3_translation <- function(a, b){
  matrix(c(1,    0,    0,    0,
           0,    1,    0,    0,
           0,    0,    1,    0,
           a[1], a[2], a[3], 1),
         ncol=4, byrow=TRUE)
}

#' 3D Euclidean, rotation about x
#'
#' @param a numeric, translation
#' @param b numeric, all other coefficients
#'
#' @return matrix 4x4
#' @keywords internal
#' @export
#' @examples
#' m3_euclidean_x(c(2, 3, 1), c(0.5, 0.2))
m3_euclidean_x <- function(a, b){
  # scaling
  phi <- sqrt(b[1]^2 + b[2]^2)

  matrix(c(phi,  0,    0,    0,
           0,    b[1], b[2], 0,
           0,   -b[2], b[1], 0,
           a[1], a[2], a[3], 1),
         ncol=4, byrow=TRUE)
}

#' 3D Euclidean, rotation about y
#'
#' @param a numeric, translation
#' @param b numeric, all other coefficients
#'
#' @return matrix 4x4
#' @keywords internal
#' @export
#' @examples
#' m3_euclidean_y(c(2, 3, 1), c(0.5, 0.2))
m3_euclidean_y <- function(a, b){
  # scaling
  phi <- sqrt(b[1]^2 + b[2]^2)

  matrix(c(b[1], 0,   -b[2], 0,
           0,    phi,  0,    0,
           b[2], 0,    b[1], 0,
           a[1], a[2], a[3], 1),
         ncol=4, byrow=TRUE)
}

#' 3D Euclidean, rotation about z
#'
#' @param a numeric, translation
#' @param b numeric, all other coefficients
#'
#' @return matrix 4x4
#' @keywords internal
#' @export
#' @examples
#' m3_euclidean_z(c(2, 3, 1), c(0.5, 0.2))
m3_euclidean_z <- function(a, b){
  # scaling
  phi <- sqrt(b[1]^2 + b[2]^2)

  matrix(c( b[1], b[2], 0,    0,
           -b[2], b[1], 0,    0,
            0,    0,    phi,  0,
            a[1], a[2], a[3], 1),
         ncol=4, byrow=TRUE)
}


#' 3D Affine
#'
#' @param a numeric, translation
#' @param b numeric, all other coefficients
#'
#' @return matrix 4x4
#' @export
#' @keywords internal
#' @examples
#' m3_affine(c(2, 3, 1), c(0.5, 0.2, 4, 2, 6, 3, 2, 5, 1))
m3_affine <- function(a, b){
  # scaling
  phi <- sqrt(b[1]^2 + b[2]^2)

  matrix(c(b[1], b[4], b[7], 0,
           b[2], b[5], b[8], 0,
           b[3], b[6], b[9], 0,
           a[1], a[2], a[3], 1),
         ncol=4, byrow=TRUE)

}

#' 3D Projective
#'
#' @param a numeric, translation
#' @param b numeric, all other coefficients
#'
#' @return matrix 4x4
#' @export
#' @keywords internal
#' @examples
#' m3_projective(c(2, 3, 1), c(0.5, 0.2, 4, 2, 6, 3, 2, 5, 1, 6, 8, 9))
m3_projective <- function(a, b){
  # scaling
  phi <- sqrt(b[1]^2 + b[2]^2)

  matrix(c(b[1], b[4], b[7], b[10],
           b[2], b[5], b[8], b[11],
           b[3], b[6], b[9], b[12],
           a[1], a[2], a[3], 1),
         ncol=4, byrow=TRUE)

}
