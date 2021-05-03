#' The 'TriDimRegression' package.
#'
#' @description Fits 2D and 3D geometric transformations. Provides posterior via Stan.
#' Includes computation of LOO and WAIC information criteria, R-squared.
#'
#' To fit transformation, call the main function either via a formula that specifies
#' dependent and independent variables with the `data` table or by supplying two tables
#' one containing all independent variables and one containing all dependent variables.
#'
#' For the 2D data, you can fit \code{"translation"} (2 parameters for translation only), \code{"euclidean"}
#' (4 parameters: 2 for translation, 1 for scaling, and 1 for rotation),
#' \code{"affine"} (6 parameters: 2 for translation and 4 that jointly describe scaling, rotation and sheer),
#' or \code{"projective"} (8 parameters: affine plus 2 additional parameters to account for projection).
#' For 3D data, you can fit \code{"translation"} (3 for translation only), \code{"euclidean_x"}, \code{"euclidean_y"},
#' \code{"euclidean_z"} (5 parameters: 3 for translation scale, 1 for rotation, and 1 for scaling),
#' \code{"affine"} (12 parameters: 3 for translation and 9 to account for scaling, rotation, and sheer),
#' and \code{"projective"} (15 parameters: affine plus 3 additional parameters to account for projection).
#' transformations. For details on transformation matrices and computation of scale and rotation parameters please
#' see \code{vignette("transformation_matrices", package = "TriDimRegression")}
#'
#' Once the data is fitted, you can extract the transformation coefficients via \code{\link{coef}} function and the matrix
#' itself via  \code{\link{transformation_matrix}}. Predicted data, either based on the original data or on the new data,
#' can be generated via  \code{\link{predict}}. Bayesian R-squared can be computed with or without adjustment via
#' \code{\link{R2}} function. In all three cases, you have choice between summary (mean + specified quantiles) or full
#' posterior samples.  \code{\link{loo}} and  \code{\link{waic}} provide corresponding measures that can be used for comparison
#' via [loo::loo_compare()] function.
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
#' \code{\link[TriDimRegression:fit_transformation]{fit_transformation}}
#' \code{\link[TriDimRegression:fit_transformation_df]{fit_transformation_df}}
#' \code{\link[TriDimRegression:tridim_transformation-class]{tridim_transformation}}
#' \code{vignette("transformation_matrices", package = "TriDimRegression")}
#' \code{vignette("calibration", package = "TriDimRegression")}
#' \code{vignette("comparing_faces", package = "TriDimRegression")}
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.19.3. https://mc-stan.org
#'
#' @examples
#' \donttest{
#' # Fitting via formula
#' euc2 <- fit_transformation(depV1 + depV2 ~ indepV1 + indepV2,
#'                            NakayaData, 'euclidean')
#' aff2 <- fit_transformation(depV1 + depV2 ~ indepV1 + indepV2,
#'                            NakayaData, 'affine')
#' prj2 <- fit_transformation(depV1 + depV2 ~ indepV1 + indepV2,
#'                            NakayaData, 'projective')
#'
#' # summary of transformation coefficients
#' coef(euc2)
#'
#' # statistical comparison via WAIC criterion
#' loo::loo_compare(waic(euc2), waic(aff2), waic(prj2))
#'
#' # Fitting via two tables
#' euc2 <- fit_transformation_df(NakayaData[, 1:2], NakayaData[, 3:4],
#'   'euclidean')
#' tr3 <- fit_transformation_df(Face3D_W070, Face3D_W097, transformation ='translation')
#' }
NULL
