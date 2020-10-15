#' The 'TriDimRegression' package.
#'
#' @description Fits 2D and 3D geometric transformations. Provides posterior via Stan.
#' Includes computation of LOO and WAIC information criteria, R-squared.
#'
#' To fit transofmration, call the main function either via a formula that specifies
#' dependent and independent variables with the `data` table or by supplying two tables
#' one containing all independent variables and one containing all dependent variables.
#'
#' For the 2D data, you can fit \code{"euclidean"} (scale, rotation, and translation),
#' \code{"affine"} (+shear), or \code{"projective"} (+tilt) transformations. For 3D data, you can fit
#' \code{"euclidean_x"}, \code{"euclidean_y"}, \code{"euclidean_z"} (scale, rotation around the
#' specified axis, and translation), \code{"euclidean"} (scale, rotation around all three axes, and translation),
#' \code{"affine"} (\code{"euclidean"} + shear for x-plane), and \code{"projective"} (+shear for y and z planes)
#' transformations.
#'
#' @docType package
#' @name TriDimRegression-package
#' @aliases TriDimRegression
#' @useDynLib TriDimRegression, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @import Formula
#' @importFrom dplyr bind_cols bind_rows %>%
#' @importFrom future availableCores
#' @importFrom glue glue
#' @importFrom purrr map map_df
#' @importFrom tidyr pivot_wider
#' @importFrom rstan sampling
#' @importFrom stats printCoefmat quantile
#' @importFrom bayesplot mcmc_intervals
#' @importFrom stats coef predict
#' @importFrom loo loo waic
#'
#' @seealso
#' \code{\link[TriDimRegression:fit_geometric_transformation]{fit_geometric_transformation}}
#' \code{\link[TriDimRegression:fit_geometric_transformation_df]{fit_geometric_transformation_df}}
#' \code{\link[TriDimRegression:tridim_transform-class]{tridim_transform}}
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.19.3. https://mc-stan.org
#'
#' @examples
#' \dontrun{
#' # Fitting via formula
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
#'
#' # Fitting via two tables
#' euc2 <- fit_geometric_transformation_df(NakayaData[, 1:2], NakayaData[, 3:4],
#'   'euclidean')
#' euc3x <- fit_geometric_transformation_df(female_face_neutral,
#'   female_face_happy, "euclidean_x")
#' }
NULL
