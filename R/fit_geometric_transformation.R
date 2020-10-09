library(Formula)

#' Fitting Bidimensional or Tridimensional Regression / Geometric Transformation Models
#'
#' @usage
#' fit_geometric_transformation(formula, data, transformation)
#'
#' @param formula a symbolic description of the model to be fitted in the format \code{Xdep + Ydep ~ Xind + Yind}, where
#' \code{Xdep} and \code{Ydep} are dependent and \code{Xind} and \code{Yind} are independent variables
#' @param data a data frame containing variables for the model.
#' @param transformation the transformation to be used, either \code{'euclidean'}, \code{'affine'}, or \code{'projective'}.
#' #' @param chains Number of chains for sampling.
#' @param cores Number of CPU cores to use for sampling. If omitted, All cores are used.
#' @param ... Additional arguments passed to [rstan::sampling()][rstan::sampling] function.
#'
#' @return returns an object of class "tridim_transform".
#' An object of class "tridim_transform" is a list containing at least the following components:
#' \item{\code{transformation}}{string with the transformation type}
#' \item{\code{Ndim}}{number of dimensions (2 or 3).}
#' \item{\code{stanfit}}{\code{\link{rstan::stanfit}} object.}
#' \item{\code{formula}}{formula, describing input and output columns}
#' \item{\code{data}}{data in Stan list, used to fit the model}
#' \item{\code{Call}}{function call information, incorporates the \code{formula}, \code{transformation}, and \code{data}.}
#' @export
#'
#' @examples
#' \dontrun{
#' # Geometric transformations of 2D data
#' euc2 <- fit_geometric_transformation(depV1 + depV2 ~ indepV1 + indepV2, NakayaData, 'euclidean')
#' aff2 <- fit_geometric_transformation(depV1 + depV2 ~ indepV1 + indepV2, NakayaData, 'affine')
#' prj2 <- fit_geometric_transformation(depV1 + depV2 ~ indepV1 + indepV2, NakayaData, 'projective')
#'
#' # summary of transformation coefficients
#' coef(euc2)
#'
#' # statistical comparison via WAIC criterion
#' loo::loo_compare(waic(euc2), waic(aff2), waic(prj2))
#' }
fit_geometric_transformation <- function(formula, data, transformation) { UseMethod("fit_geometric_transformation") }

#' @export
fit_geometric_transformation.formula <-  function(formula, data, transformation, chains=4, cores=NULL, ...){

  ## --------------- Check that arguments are present  ---------------
  if(missing(formula)) stop("formula is missing or incorrect")
  if (missing(data)) stop('data is missing')
  if (missing(transformation)) stop('transformation argument is missing')

  # placeholder for the fit object
  tridim <- list(Call=match.call(expand.dots = FALSE))
  # tridim$formula <- tridim$Call[2]
  attr(tridim, "class") <- "tridim_transform"

  ## --------------- Check that dependent and independent variables are valid  ---------------
  if (!is.data.frame(data)) stop("data parameter is not a data.frame")
  model_formula <- Formula::Formula(formula)
  tridim$formula <- model_formula
  tridim$data <- list(rowsN = nrow(data),
                      dv = as.matrix(Formula::model.part(model_formula, data = data, lhs = 1)),
                      iv = as.matrix(Formula::model.part(model_formula, data = data, rhs = 1)))

  if (ncol(tridim$data$dv) != ncol(tridim$data$iv)) stop("Different number of dependent and independent variables.")
  if (!ncol(tridim$data$dv) %in% c(2, 3)) stop("Wrong number of variables (%d), can transform only 2D or 3D data.")
  TriDimRegression::check_variables(tridim$data$dv, "Dependent variables")
  TriDimRegression::check_variables(tridim$data$iv, "Independent variables")
  tridim$dimN <- ncol(tridim$data$dv)
  tridim$data$varsN <- ncol(tridim$data$dv)

  ## --------------- Figuring out which transformations are allowed and whether it matches the requested one ---------------
  valid_transformations <- c("euclidean"=1, "affine"=2, "projective"=3)
  if (tridim$dimN == 3){
    valid_transformations <-c(valid_transformations, "euclidean_x"= 4, "euclidean_y" = 5, "euclidean_z" = 6)
  }
  if (!transformation %in% names(valid_transformations)) stop(sprintf("Unknown or inapplicable transformation '%s'", transformation))
  tridim$data$transform = valid_transformations[transformation]
  tridim$transformation <- transformation

  ## --------------- Fit ---------------
  stanmodel_names <- c(NA, "bi", "tri")
  tridim$stanfit <- rstan::sampling(stanmodels[[stanmodel_names[tridim$dimN]]],
                                    data=tridim$data,
                                    chains=chains,
                                    cores=ifelse(is.null(cores), future::availableCores(), cores),
                                    ...)
  tridim
}
