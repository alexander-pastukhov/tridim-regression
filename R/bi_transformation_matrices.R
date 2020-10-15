#' 2D Rotation Matrix
#'
#' @param theta numeric, rotation angle
#'
#' @return matrix 3x3
#' @export
#' @examples
#' bi_rotation_matrix(pi/4)
bi_rotation_matrix <- function(theta){
  stopifnot(is.finite(theta), !is.null(theta))
  matrix(c(cos(theta), sin(theta), 0,
          -sin(theta), cos(theta), 0,
          0, 0, 1),
         ncol=3, byrow=TRUE)
}

#' 2D Translation Matrix
#'
#' @param dx numeric, horizontal component
#' @param dy numeric, vertical component
#'
#' @return matrix 3x3
#' @export
#'
#' @examples
#' bi_translation_matrix(1, -1)
bi_translation_matrix <- function(dx, dy){
  stopifnot(is.finite(dx), !is.null(dx), is.finite(dy), !is.null(dy))
  matrix(c(1, 0, 0,
           0, 1, 0,
           dx, dy, 1),
         ncol=3, byrow=TRUE)
}

#' 2D Scaling Matrix
#'
#' @param sx numeric, X-scale
#' @param sy numeric Y-scale
#'
#' @return matrix \[3✕3\]
#' @export
#'
#' @examples
#' bi_scale_matrix(2, 0.5)
bi_scale_matrix <- function(sx, sy){
  stopifnot(is.finite(sx), !is.null(sx), is.finite(sy), !is.null(sy))
  matrix(c(sx, 0, 0,
           0, sy, 0,
           0,  0, 1),
         ncol=3, byrow=TRUE)
}

#' 2D Shearing Matrix
#'
#' @param shx numeric, x-shear
#' @param shy numeric, y-shear
#'
#' @return matrix 3x3
#' @export
#'
#' @examples
#' bi_shear_matrix(0, 2)
bi_shear_matrix <- function(shx, shy){
  stopifnot(is.finite(shx), !is.null(shx), is.finite(shy), !is.null(shy))
  matrix(c(1, shy, 0,
           shx, 1, 0,
           0, 0, 1),
         ncol=3, byrow=TRUE)
}


#' 2D Tilt Matrix
#'
#' @param e tilt parameter
#' @param f tilt parameter
#'
#' @return matrix 3x3
#' @export
#'
#' @examples
#' bi_tilt_matrix(0, 1)
bi_tilt_matrix <- function(e, f){
  stopifnot(is.finite(e), !is.null(e), is.finite(f), !is.null(f))
  matrix(c(1, 0, e,
           0, 1, f,
           0, 0, 1),
         ncol=3, byrow=TRUE)
}
