#' The 'TriDimRegression' package.
#'
#' @description Fits 2D and 3D geometric transformations. Provides posterior via Stan.
#' Includes computation of LOO and WAIC information criteria, R-squared.
#'
#' @docType package
#' @name TriDimRegression-package
#' @aliases TriDimRegression
#' @useDynLib TriDimRegression, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @import Formula
#' @importFrom dplyr bind_cols bind_rows
#' @importFrom future availableCores
#' @importFrom glue glue
#' @importFrom purrr map map_df
#' @importFrom tidyr pivot_wider
#' @importFrom rstan sampling
#'
#' @references
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.19.3. https://mc-stan.org
#'
NULL
