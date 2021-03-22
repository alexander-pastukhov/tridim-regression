#' Fitting Bidimensional or Tridimensional Regression / Geometric Transformation Models via Two Tables.
#'
#' @description
#' Fits Bidimensional or Tridimensional regression / geometric transformation models using
#' Stan engine. Two sets of coordinates are supplied via \code{iv} (for an independent variable)
#' and \code{dv} (for the dependent one). The two tables must have the same dimensions
#' (both N×2 or N×3).
#'
#' For the 2D data, you can fit \code{"translation"} (2 for translation only), \code{"euclidean"}
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
#' @param iv a data frame containing independent variable, must by numeric only, N×2 or N×3.
#' @param dv a data frame containing dependent variable, must by numeric only, N×2 or N×3.
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
#' @export
#' @seealso \code{\link{fit_transformation}}
#' @examples
#' \dontrun{
#' # Geometric transformations of 2D data
#' euc2 <- fit_transformation_df(NakayaData[, 1:2], NakayaData[, 3:4],
#'   'euclidean')
#' euc3x <- fit_transformation_df(female_face_neutral,
#'   female_face_happy, "euclidean_x")
#' }

fit_transformation_df <- function(iv, dv, transformation, priors=NULL, chains=1, cores=NULL, ...) {
  if (!is.data.frame(dv) && !is.matrix(dv)) stop("dv must be a data.frame or a matrix")
  if (!is.data.frame(iv) && !is.matrix(iv)) stop("iv must be a data.frame or a matrix")

  tridim <- tridim_transformation(transformation, as.matrix(iv), as.matrix(dv), formula=NULL, prior=priors)

  # fitting function
  tridim$stanfit <- rstan::sampling(tridim$stanmodel,
                                    data=tridim$data,
                                    chains=chains,
                                    cores=ifelse(is.null(cores), future::availableCores(), cores),
                                    ...)
  tridim
}
