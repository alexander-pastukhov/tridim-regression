#' Class \code{tridim_transform} of geometric transformations
#' fitted with the \code{\link[TriDimRegression:fit_geometric_transformation]{fit_geometric_transformation}} function.
#'
#' Geometric transformations fitted with the
#' \code{\link[TriDimRegression:fit_geometric_transformation]{fit_geometric_transformation}} function
#' represented as a \code{tridim_transform} object with information about transformation, data dimension,
#' call formula, and fitted \code{\link[rstan:stanfit]{stanfit}} object,
#'
#' @name tridim_transform-class
#' @aliases tridim_transform
#' @docType class
#'
#' @details
#' See \code{methods(class = "tridim_transform")} for an overview of available methods.
#'
#' @slot transformation A \code{string} with the transformation name.
#' @slot formula A \code{\link[Formula:formula]{formula}} object.
#' @slot Ndim An \code{integer} with data dimension, either \code{2} or \code{3}.
#' @slot data A \code{list} containing variables used for the \code{\link[rstan:sampling]{sampling}}.
#' @slot stanmodel A \code{\link[rstan:stan_model]{stan_model}} used for sampling.
#'
#' @seealso
#'   \code{\link{fit_geometric_transformation}}
NULL

# tridim_transform class
tridim_transform <- function(transformation,
                             iv, dv,
                             formula=NULL) {

  # data dimensions consistency
  if (ncol(dv) != ncol(iv)) stop("Different number of columns in iv and dv")
  if (ncol(dv) < 2 || ncol(dv) > 3) stop("Number of columns in data.frames must be either 2 or 3")
  if (nrow(dv) != nrow(iv)) stop("Different number of rows in iv and dv")

  # data must be numeric
  if (!is.numeric(as.matrix(dv))) stop("Non-numeric dv")
  if (!is.numeric(as.matrix(iv))) stop("Non-numeric iv")

  # data must be finite
  if (sum(!is.finite(as.matrix(dv))) > 0) stop("dv has non-finite elements")
  if (sum(!is.finite(as.matrix(iv))) > 0) stop("iv has non-finite elements")

  # transformation must match data dimensions
  valid_transformations <- c("euclidean"=1, "affine"=2, "projective"=3)
  if (ncol(dv) == 3){
    valid_transformations <-c(valid_transformations, "euclidean_x"= 4, "euclidean_y" = 5, "euclidean_z" = 6)
  }
  if (!transformation %in% names(valid_transformations)) stop(sprintf("Unknown or inapplicable transformation '%s'", transformation))

  # if formula is given, we deduce it from data.frame names
  if (is.null(formula)) {
    formula <- as.Formula(paste(paste0(colnames(dv), collapse = " + "),
                                paste0(colnames(iv), collapse = " + "),
                                sep = " ~ "))
  }

  # object without data
  stanmodel_names <- c(NA, "bi", "tri")
  object <- list(transformation = transformation,
                 dimN = ncol(dv),
                 formula = formula,
                 stanmodel = stanmodels[[stanmodel_names[ncol(dv)]]])

  # creating stan data list
  object$data <- list(transform = valid_transformations[transformation],
                      rowsN = nrow(dv),
                      varsN = ncol(dv),
                      dv = dv,
                      iv = iv)

  class(object) <- "tridim_transform"
  object
}


#' Checks if argument is a \code{tridim_transform} object
#'
#' @param x An \R object
#'
#' @return Logical
#' @export
is.tridim_transform <- function(x){
  inherist(x, "tridim_transform")
}
