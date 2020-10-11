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
#' fit_geometric_transformation(iv, dv, transformation, iv_prefix="iv_", dv_prefix="dv_", chains, cores, ...)
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
fit_geometric_transformation <- function(object, ...) { UseMethod("fit_geometric_transformation") }

#' @export
fit_geometric_transformation.data.frame <- function(dv, iv, transformation, iv_prefix="iv_", dv_prefix="dv_", ...) {
  if (!is.data.frame(dv) && !is.matrix(dv)) stop("dv must be a data.frame or a matrix")
  if (!is.data.frame(iv) && !is.matrix(iv)) stop("iv must be a data.frame or a matrix")
  if (ncol(dv) != ncol(iv)) stop("Different number of columns in iv and dv")
  if (ncol(dv) < 2 || ncol(dv) > 3) stop("Number of columns is data.frames must be either 2 or 3")
  if (nrow(dv) != nrow(iv)) stop("Different number of rows in iv and dv")
  if (!is.character(iv_prefix)) stop("Invalid iv_prefix")
  if (!is.character(dv_prefix)) stop("Invalid dv_prefix")
  if (!is.numeric(as.matrix(dv))) stop("Non-numeric dv")
  if (!is.numeric(as.matrix(iv))) stop("Non-numeric iv")

  # adding prefix to column names
  iv_names <- c(glue::glue("{iv_prefix}{colnames(iv)}"))
  dv_names <- c(glue::glue("{dv_prefix}{colnames(dv)}"))
  if (length(intersect(iv_names, dv_names)) > 0) stop("Matching names between iv and dv, despite prefixes")
  names(iv) <- iv_names
  names(dv) <- dv_names

  # formula
  transform_formula <- as.Formula(paste(paste0(dv_names, collapse = " + "),
                                        paste0(iv_names, collapse = " + "),
                                        sep = " ~ "))

  # passing data to the main function
  fit_geometric_transformation.formula(transform_formula, cbind(dv, iv), transformation, ...)
}


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
