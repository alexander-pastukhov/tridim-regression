#' Fitting Bidimensional or Tridimensional Regression / Geometric Transformation Models.
#'
#' @description
#' Fits Bidimensional or Tridimensional regression / geometric transformation models using
#' Stan engine. Can be used either via a formula interface or by supplying two tables, one
#' for an indepdent variable (\code{iv}) and one for the dependent one (\code{dv}).
#' In the latter case, the two tables must have the same dimenisons (both N x 2 or N x 3).
#'
#' @usage
#' fit_geometric_transformation(formula, data, transformation, chains, cores, ...)
#' fit_geometric_transformation(iv, dv, transformation,
#'   iv_prefix="iv_", dv_prefix="dv_", chains, cores, ...)
#'
#' @name fit_geometric_transformation
#'
#' @param formula a symbolic description of the model to be fitted in the format \code{Xdep + Ydep ~ Xind + Yind}, where
#' \code{Xdep} and \code{Ydep} are dependent and \code{Xind} and \code{Yind} are independent variables
#' @param data a data frame containing variables for the model.
#' @param transformation the transformation to be used, either \code{'euclidean'}, \code{'affine'}, or \code{'projective'}.
#' @param iv a data frame containing independent variable, must by numeric only, Nx2 or Nx3.
#' @param dv a data frame containing dependent variable, must by numeric only, Nx2 or Nx3.
#' @param iv_prefix string used to disambiguate column names for independent variable, defaults to "iv_".
#' @param dv_prefix string used to disambiguate column names for dependent variable, defaults to "dv_".
#' @param chains Number of chains for sampling.
#' @param cores Number of CPU cores to use for sampling. If omitted, all available cores are used.
#' @param ... Additional arguments passed to [rstan::sampling()](rstan::sampling) function.
#'
#' @return returns an object of class "tridim_transform".
#' An object of class "tridim_transform" is a list containing at least the following components:
#' \item{\code{transformation}}{string with the transformation type}
#' \item{\code{Ndim}}{number of dimensions (2 or 3).}
#' \item{\code{stanfit}}{\code{\link[rstan]{stanfit}} object.}
#' \item{\code{formula}}{formula, describing input and output columns}
#' \item{\code{data}}{data in Stan list, used to fit the model}
#' \item{\code{Call}}{function call information, incorporates the \code{formula}, \code{transformation}, and \code{data}.}
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

#' @export
fit_geometric_transformation <- function(dv, ...) { UseMethod("fit_geometric_transformation") }

#' @export
fit_geometric_transformation.default <- function(iv, dv, transformation, iv_prefix="iv_", dv_prefix="dv_", chains=4, cores=NULL, ...) {
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
