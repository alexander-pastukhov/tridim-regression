#' Class \code{tridim_transform}.
#'
#' Geometric transformations fitted with the
#' \code{\link[TriDimRegression:fit_geometric_transformation]{fit_geometric_transformation}} function
#' represented as a \code{tridim_transform} object with information about transformation, data dimension,
#' call formula, and fitted \code{\link[rstan:stanfit-class]{stanfit}} object,
#'
#' @name tridim_transform-class
#' @aliases tridim_transform
#' @docType class
#'
#' @details
#' See \code{methods(class = "tridim_transform")} for an overview of available methods.
#'
#' @slot transformation A \code{string} with the transformation name.
#' @slot formula A \code{\link[Formula:formula.Formula]{formula}} object.
#' @slot Ndim An \code{integer} with data dimension, either \code{2} or \code{3}.
#' @slot data A \code{list} containing variables used for the \code{\link[rstan:sampling]{sampling}}.
#' @slot stanmodel A \code{\link[rstan:stanmodel-class]{stanmodel}} used for sampling.
#' @slot stanfit a \code{\link[rstan:stanfit-class]{stanfit}} object.
#'
#' @seealso
#'   \code{\link{fit_geometric_transformation}}
NULL

# tridim_transform class
tridim_transform <- function(transformation,
                             iv, dv,
                             formula=NULL,
                             priors) {

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

  # computing means as guidance priors
  iv_means = rep(apply(iv, FUN=mean, MARGIN=2), times = ncol(iv))
  dv_means = rep(apply(dv, FUN=mean, MARGIN=2), each=ncol(iv))

  # default priors
  prior_defaults <- list(
    "scale" =  c(0, max(abs(dv_means/iv_means)) / 4),
    "sheer" = c(0, max(abs(dv_means/iv_means)) / 4),
    "rotation" = c(0, pi/2), # overwritten by uniform in reality
    "tilt" = c(0, 10),
    "translation" = c(0, max(abs(dv_means- iv_means)) / 2)
  )

  # checking all available priors for validity
  for(param_prior_name in names(prior_defaults)){
    if (param_prior_name %in% names(priors)) {
      check_normal_prior(priors[param_prior_name], param_prior_name)
      object$data[[sprintf('%s_prior', param_prior_name)]] <- unname(priors[[param_prior_name]])
    }
    else {
      object$data[[sprintf('%s_prior', param_prior_name)]] <- unname(prior_defaults[[param_prior_name]])
    }
  }

  # special cases
  object$data[["normal_rotation_prior"]] <- as.integer("rotation" %in% names(priors))
  if ("sigma" %in% names(priors)) {
    check_exponential_prior(priors["sigma"], "sigma")
    object$data[["sigma_prior"]] <- unname(priors[["sigma"]])
  }
  else {
    object$data[["sigma_prior"]] <- 1
  }

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
  inherits(x, "tridim_transform")
}
