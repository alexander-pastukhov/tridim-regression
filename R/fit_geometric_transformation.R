#' Fitting Bidimensional or Tridimensional Regression / Geometric Transformation Models via Formula.
#'
#' @description
#' Fits Bidimensional or Tridimensional regression / geometric transformation models using
#' Stan engine. The \code{formula} described dependent and independent numeric variables in the
#' \code{data}.
#'
#' For the 2D data, you can fit \code{"euclidean"} (scale, rotation, and translation),
#' \code{"affine"} (+shear), or \code{"projective"} (+tilt) transformations. For 3D data, you can fit
#' \code{"euclidean_x"}, \code{"euclidean_y"}, \code{"euclidean_z"} (scale, rotation around the
#' specified axis, and translation), \code{"euclidean"} (scale, rotation around all three axes, and translation),
#' \code{"affine"} (\code{"euclidean"} + shear for x-plane), and \code{"projective"} (+shear for y and z planes)
#' transformations.
#'
#' @param formula a symbolic description of the model to be fitted in the format \code{Xdep + Ydep ~ Xind + Yind}, where
#' \code{Xdep} and \code{Ydep} are dependent and \code{Xind} and \code{Yind} are independent variables
#' @param data a data frame containing variables for the model.
#' @param transformation the transformation to be used, either \code{'euclidean'}, \code{'affine'}, or \code{'projective'}.
#' @param chains Number of chains for sampling.
#' @param cores Number of CPU cores to use for sampling. If omitted, all available cores are used.
#' @param ... Additional arguments passed to \code{\link[rstan:sampling]{sampling}} function.
#'
#' @name fit_geometric_transformation
#' @seealso \code{\link{fit_geometric_transformation_df}}
#' @examples
#' \dontrun{
#' # Geometric transformations of 2D data
#' euc2 <- fit_geometric_transformation(depV1 + depV2 ~ indepV1 + indepV2,
#'                                      NakayaData, 'euclidean')
#' aff2 <- fit_geometric_transformation(depV1 + depV2 ~ indepV1 + indepV2,
#'                                      NakayaData, 'affine')
#' prj2 <- fit_geometric_transformation(depV1 + depV2 ~ indepV1 + indepV2,
#'                                      NakayaData, 'projective')
#'
#' # summary of transformation coefficients
#' coef(euc2)
#'
#' # statistical comparison via WAIC criterion
#' loo::loo_compare(waic(euc2), waic(aff2), waic(prj2))
#' }
#' @export
fit_geometric_transformation.formula <-  function(formula, data, transformation, chains=4, cores=NULL, ...){
  ## --------------- Check that dependent and independent variables are valid  ---------------
  if (!is.data.frame(data)) stop("data parameter is not a data.frame")

  model_formula <- Formula::Formula(formula)
  tridim <- tridim_transform(transformation,
                             iv=as.matrix(Formula::model.part(model_formula, data = data, rhs = 1)),
                             dv=as.matrix(Formula::model.part(model_formula, data = data, lhs = 1)),
                             formula=model_formula)

  # fitting function
  tridim$stanfit <- rstan::sampling(tridim$stanmodel,
                                    data=tridim$data,
                                    chains=chains,
                                    cores=ifelse(is.null(cores), future::availableCores(), cores),
                                    ...)
  tridim
}

#' @export
fit_geometric_transformation <- function(formula, ...) {UseMethod("fit_geometric_transformation")}
