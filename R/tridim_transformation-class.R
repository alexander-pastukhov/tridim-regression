#' Class \code{tridim_transformation}.
#'
#' Geometric transformations fitted with the
#' \code{\link[TriDimRegression:fit_transformation]{fit_transformation}} function
#' represented as a \code{tridim_transformation} object with information about transformation, data dimension,
#' call formula, and fitted \code{\link[rstan:stanfit-class]{stanfit}} object,
#'
#' @name tridim_transformation-class
#' @aliases tridim_transformation
#' @docType class
#'
#' @details
#' See \code{methods(class = "tridim_transformation")} for an overview of available methods.
#'
#' @slot transformation A \code{string} with the transformation name.
#' @slot formula A \code{\link[Formula:formula.Formula]{formula}} object.
#' @slot Ndim An \code{integer} with data dimension, either \code{2} or \code{3}.
#' @slot data A \code{list} containing variables used for the \code{\link[rstan:sampling]{sampling}}.
#' @slot stanmodel A \code{\link[rstan:stanmodel-class]{stanmodel}} used for sampling.
#' @slot stanfit a \code{\link[rstan:stanfit-class]{stanfit}} object.
#'
#' @seealso
#'   \code{\link{fit_transformation}}
NULL

# tridim_transform class
tridim_transformation <- function(transformation,
                                  iv, dv,
                                  formula=NULL,
                                  priors=NULL) {

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
  if (ncol(dv) == 2){
    valid_transformations <- c("translation"=0, "euclidean"=1, "affine"=2, "projective"=3)
  }
  else {
    valid_transformations <-c("translation"=0, "affine"=2, "projective"=3, "euclidean_x"= 4, "euclidean_y" = 5, "euclidean_z" = 6)
  }
  if (!transformation %in% names(valid_transformations)) stop(sprintf("Unknown or inapplicable transformation '%s'", transformation))

  # if formula is given, we deduce it from data.frame names
  if (is.null(formula)) {
    if (is.null(colnames(dv))) colnames(dv) <- c("xd", "yd", "zd")[1:ncol(transformed_face)]
    if (is.null(colnames(iv))) colnames(iv) <- c("xi", "yi", "zi")[1:ncol(transformed_face)]

    formula <- as.Formula(paste(paste0(colnames(dv), collapse = " + "),
                                paste0(colnames(iv), collapse = " + "),
                                sep = " ~ "))
  }

  # object without data
  object <- list(transformation = transformation,
                 dimN = ncol(dv),
                 formula = formula,
                 stanmodel = stanmodels$tridim_transformation)

  # creating stan data list
  object$data <- list(transform = valid_transformations[transformation],
                      rowsN = nrow(dv),
                      varsN = ncol(dv),
                      dv = dv,
                      iv = iv,
                      dv_sd = apply(NakayaData[, 3:4], MARGIN=2, FUN=sd))

  # figuring out the number of parameters IN ADDITION to translation
  if (ncol(dv) == 2){
    param_n <- c("translation"=0, "euclidean"=2, "affine"=4, "projective"=6)
  }
  else {
    param_n <- -c("translation"=0, "euclidean_x"= 2, "euclidean_y" = 2, "euclidean_z" = 2, "affine"=9, "projective"=12)
  }
  object$data[["betaN"]] <- get_beta_n(ncol(dv), transformation)

  # computing means as guidance priors
  iv_means <- rep(apply(iv, FUN=mean, MARGIN=2), times=ncol(iv))
  dv_means <- rep(apply(dv, FUN=mean, MARGIN=2), each=ncol(iv))

  # default priors
  prior_defaults <- list(
    "beta" =  c(0, max(abs(dv_means/iv_means)) / 2),
    "translation" = c(0, max(abs(dv_means- iv_means)))
  )

  # checking all available priors for validity
  for(param_prior_name in names(prior_defaults)){
    # checking that defaults make sense, that sigma is
    if ((prior_defaults[[param_prior_name]][2] < 1e-5) ||
        is.nan(prior_defaults[[param_prior_name]][2])){
      prior_defaults[[param_prior_name]][2] <- 1e-5
    }

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

  class(object) <- "tridim_transformation"
  object
}


#' Checks if argument is a \code{tridim_transformation} object
#'
#' @param x An \R object
#'
#' @return Logical
#' @export
is.tridim_transformation <- function(x){
  inherits(x, "tridim_transformation")
}
