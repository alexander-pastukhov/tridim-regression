#' 3D Rotation around x-axis
#'
#' @param theta Rotation angle
#'
#' @return matrix 4x4
#' @export
#'
#' @examples
#' tri_rotation_x_matrix(pi/4)
tri_rotation_x_matrix <- function(theta) {
  stopifnot(is.finite(theta), !is.null(theta))
  matrix(c(1, 0, 0, 0,
           0, cos(theta), sin(theta), 0,
           0, -sin(theta), cos(theta), 0,
           0, 0, 0, 1),
         ncol=4, byrow=TRUE)
}

#' 3D Rotation around y-axis
#'
#' @param theta Rotation angle
#'
#' @return matrix 4x4
#' @export
#'
#' @examples
#' tri_rotation_y_matrix(pi/4)
tri_rotation_y_matrix <- function(theta) {
  stopifnot(is.finite(theta), !is.null(theta))
  matrix(c(cos(theta), 0, -sin(theta), 0,
           0, 1, 0, 0,
           sin(theta), 0, cos(theta), 0,
           0, 0, 0, 1),
         ncol=4, byrow=TRUE)
}

#' 3D Rotation around z-axis
#'
#' @param theta Rotation angle
#'
#' @return matrix 4x4
#' @export
#'
#' @examples
#' tri_rotation_z_matrix(pi/4)
tri_rotation_z_matrix <- function(theta) {
  stopifnot(is.finite(theta), !is.null(theta))
  matrix(c(cos(theta), sin(theta), 0, 0,
           -sin(theta), cos(theta), 0, 0,
           0, 0, 1, 0,
           0, 0, 0, 1),
         ncol=4, byrow=TRUE)
}


#' 3D Shear for x-axis
#'
#' @param shx_y Shear for y-axis
#' @param shx_z Shear for z-axis
#'
#' @return matrix 4x4
#' @export
#'
#' @examples
#' tri_shear_x_matrix(2, 0.5)
tri_shear_x_matrix <- function(shx_y, shx_z) {
  stopifnot(is.finite(shx_y), !is.null(shx_y), is.finite(shx_z), !is.null(shx_z))
  matrix(c(1, 0, 0, 0,
           shx_y, 1, 0, 0,
           shx_z, 0, 1, 0,
           0, 0, 0, 1),
         ncol=4, byrow=TRUE)
}

#' 3D Shear for y-axis
#'
#' @param shx_x Shear for x-axis
#' @param shx_z Shear for z-axis
#'
#' @return matrix 4x4
#' @export
#'
#' @examples
#' tri_shear_y_matrix(2, 0.5)
tri_shear_y_matrix <- function(shy_x, shy_z) {
  stopifnot(is.finite(shx_x), !is.null(shx_x), is.finite(shx_z), !is.null(shx_z))
  matrix(c(1, shy_x, 0, 0,
           0, 1, 0, 0,
           0, shy_z, 1, 0,
           0, 0, 0, 1),
         ncol=4, byrow=TRUE)
}

#' 3D Shear for z-axis
#'
#' @param shx_x Shear for x-axis
#' @param shx_y Shear for z-axis
#'
#' @return matrix 4x4
#' @export
#'
#' @examples
#'tri_shear_z_matrix(2, 0.5)
tri_shear_z_matrix <- function(shz_x, shz_y) {
  stopifnot(is.finite(shx_x), !is.null(shx_x), is.finite(shx_y), !is.null(shx_y))
  matrix(c(1, 0, shz_x, 0,
           0, 1, shz_y, 0,
           0, 0, 1, 0,
           0, 0, 0, 1),
         ncol=4, byrow=TRUE)
}


#' 3D Translation
#'
#' @param dx x translation
#' @param dy y translation
#' @param dz z translation
#'
#' @return matrix 4x4
#' @export
#'
#' @examples
#' tri_translation_matrix(0, 10, 1)
tri_translation_matrix <- function(dx, dy, dz){
  stopifnot(is.finite(dx), !is.null(dx), is.finite(dy), !is.null(dy), is.finite(dz), !is.null(dz))
  matrix(c(1, 0, 0, 0,
           0, 1, 0, 0,
           0, 0, 1, 0,
           dx, dy, dz, 1),
         ncol=4, byrow=TRUE)
}

#' 3D scale matrix
#'
#' @param sx x scaling
#' @param sy y scaling
#' @param sz z scaling
#'
#' @return matrix 4x4
#' @export
#'
#' @examples
#' tri_scale_matrix(1, 2, 0.5)
tri_scale_matrix <- function(sx, sy, sz){
  stopifnot(is.finite(sx), !is.null(sx), is.finite(sy), !is.null(sy), is.finite(sz), !is.null(sz))
  matrix(c(sx, 0,  0,  0,
           0,  sy, 0,  0,
           0,  0,  sz, 0,
           0,  0,  0,  1),
         ncol=4, byrow=TRUE)
}

