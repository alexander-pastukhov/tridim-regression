#' Fitting Bidimensional or Tridimensional Regression / Geometric Transformation Models via Formula.
#'
#' @description
#' Fits Bidimensional or Tridimensional regression / geometric transformation models using
#' Stan engine. The \code{formula} described dependent and independent numeric variables in the
#' \code{data}. See also \code{\link{fit_transformation_df}}.
#'
#' For the 2D data, you can fit \code{"translation"} (2 parameters for translation only), \code{"euclidean"}
#' (4 parameters: 2 for translation, 1 for scaling, and 1 for rotation),
#' \code{"affine"} (6 parameters: 2 for translation and 4 that jointly describe scaling, rotation and sheer),
#' or \code{"projective"} (8 parameters: affine plus 2 additional parameters to account for projection).
#'
#' For 3D data, you can fit \code{"translation"} (3 for translation only), \code{"euclidean_x"}, \code{"euclidean_y"},
#' \code{"euclidean_z"} (5 parameters: 3 for translation scale, 1 for rotation, and 1 for scaling),
#' \code{"affine"} (12 parameters: 3 for translation and 9 to account for scaling, rotation, and sheer),
#' and \code{"projective"} (15 parameters: affine plus 3 additional parameters to account for projection).
#' transformations.
#'
#' For details on transformation matrices and computation of scale and rotation parameters please
#' see \code{vignette("transformation_matrices", package = "TriDimRegression")}
#'
#' @param formula a symbolic description of the model to be fitted in the format \code{Xdep + Ydep ~ Xind + Yind}, where
#' \code{Xdep} and \code{Ydep} are dependent and \code{Xind} and \code{Yind} are independent variables
#' @param data a data frame containing variables for the model.
#' @param transformation the transformation to be used: \code{"translation"} (both 2D and 3D), \code{"euclidean"} (2D),
#'   \code{"euclidean_x"}, \code{"euclidean_y"}, \code{"euclidean_z"} (3D, rotation about, respectively, x, y, and z axis),
#'   \code{"affine"} (2D and 3D), or \code{"projective"} (2D and 3D).
#' @param priors named list of parameters for prior distributions of parameters \code{a}
#'   (translation, normal distribution), \code{b} (all other parameters, normal distribution),
#'   and \code{sigma} (residual variance, exponential). E.g., \code{list("a" = c(0, 10), "b"= c(0, 1), "sigma"=1)}.
#'   Default priors are \code{"a" = c(0, max_absolute_difference_in_means(d, iv)) / 2)},
#'   \code{"b" = c(0, max_absolute_difference_in_means(d, iv)) / 2)}, \code{"sigma" = 1 * sd(dv)}.
#' @param chains Number of chains for sampling.
#' @param cores Number of CPU cores to use for sampling. If omitted, all available cores are used.
#' @param ... Additional arguments passed to \code{\link[rstan:sampling]{sampling}} function.
#' @return A [tridim_transformation][tridim_transformation-class()] object
#' @name fit_transformation
#' @seealso \code{\link{fit_transformation_df}}
#' @examples
#' \donttest{
#' # Geometric transformations of 2D data
#' euc2 <- fit_transformation(depV1 + depV2 ~ indepV1 + indepV2,
#'                                      NakayaData, 'euclidean')
#' aff2 <- fit_transformation(depV1 + depV2 ~ indepV1 + indepV2,
#'                                      NakayaData, 'affine')
#' prj2 <- fit_transformation(depV1 + depV2 ~ indepV1 + indepV2,
#'                                      NakayaData, 'projective')
#'
#' # summary of transformation coefficients
#' coef(euc2)
#'
#' # statistical comparison via WAIC criterion
#' loo::loo_compare(waic(euc2), waic(aff2), waic(prj2))
#' }
#' @export
fit_transformation.formula <-  function(formula, data, transformation, priors=NULL, chains=1, cores=NULL, ...){
  ## --------------- Check that dependent and independent variables are valid  ---------------
  if (!is.data.frame(data)) stop("data parameter is not a data.frame")

  model_formula <- Formula::Formula(formula)
  tridim <- tridim_transformation(transformation,
                                  iv=as.matrix(Formula::model.part(model_formula, data = data, rhs = 1)),
                                  dv=as.matrix(Formula::model.part(model_formula, data = data, lhs = 1)),
                                  formula=model_formula,
                                  priors=priors)

  # fitting function
  tridim$stanfit <- rstan::sampling(tridim$stanmodel,
                                    data=tridim$data,
                                    chains=chains,
                                    cores=ifelse(is.null(cores), future::availableCores(), cores),
                                    ...)
  tridim
}

#' @export
#' @keywords internal
fit_transformation <- function(formula, ...) {UseMethod("fit_transformation")}
