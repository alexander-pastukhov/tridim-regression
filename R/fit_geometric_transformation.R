#' Fitting Bidimensional or Tridimensional Regression / Geometric Transformation Models.
#'
#' @description
#' Fits Bidimensional or Tridimensional regression / geometric transformation models using
#' Stan engine. Can be used either via a formula interface or by supplying two tables, one
#' for an indepdent variable (\code{iv}) and one for the dependent one (\code{dv}).
#' In the latter case, the two tables must have the same dimenisons (both N x 2 or N x 3).
#'
#' @name fit_geometric_transformation
#' @aliases fit_geometric_transformation fit_geometric_transformation.default fit_geometric_transformation.formula
#'
#' @return returns an object of class \code{\link[TriDimRegression:tridim_transform-class]{tridim_transform-class}}.
#' @export
#'
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
NULL

#' @usage fit_geometric_transformation(iv, dv, transformation, chains, cores, ...)
#' @param iv a data frame containing independent variable, must by numeric only, Nx2 or Nx3.
#' @param dv a data frame containing dependent variable, must by numeric only, Nx2 or Nx3.
#' @param transformation the transformation to be used, either \code{'euclidean'}, \code{'affine'}, or \code{'projective'}.
#' @param chains Number of chains for sampling.
#' @param cores Number of CPU cores to use for sampling. If omitted, all available cores are used.
#' @param ... Additional arguments passed to \code{\link[rstan:sampling]{sampling}} function.
#' @export
fit_geometric_transformation.default <- function(iv, dv, transformation, chains=4, cores=NULL, ...) {
  if (!is.data.frame(dv) && !is.matrix(dv)) stop("dv must be a data.frame or a matrix")
  if (!is.data.frame(iv) && !is.matrix(iv)) stop("iv must be a data.frame or a matrix")

  tridim <- tridim_transform(transformation, iv, dv)

  # fitting function
  tridim$stanfit <- rstan::sampling(tridim$stanmodel,
                                    data=tridim$data,
                                    chains=chains,
                                    cores=ifelse(is.null(cores), future::availableCores(), cores),
                                    ...)
  tridim
}


#' @usage fit_geometric_transformation(formula, data, transformation, chains, cores, ...)
#' @param formula a symbolic description of the model to be fitted in the format \code{Xdep + Ydep ~ Xind + Yind}, where
#' \code{Xdep} and \code{Ydep} are dependent and \code{Xind} and \code{Yind} are independent variables
#' @param data a data frame containing variables for the model.
#' @param transformation the transformation to be used, either \code{'euclidean'}, \code{'affine'}, or \code{'projective'}.
#' @param chains Number of chains for sampling.
#' @param cores Number of CPU cores to use for sampling. If omitted, all available cores are used.
#' @param ... Additional arguments passed to \code{\link[rstan:sampling]{sampling}} function.
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
fit_geometric_transformation <- function(iv, ...) { UseMethod("fit_geometric_transformation") }
