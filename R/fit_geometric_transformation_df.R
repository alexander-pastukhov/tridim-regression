#' Fitting Bidimensional or Tridimensional Regression / Geometric Transformation Models via Two Tables.
#'
#' @description
#' Fits Bidimensional or Tridimensional regression / geometric transformation models using
#' Stan engine. Two sets of coordinates are supplied via \code{iv} (for an independent variable)
#' and \code{dv} (for the dependent one). The two tables must have the same dimensions
#' (both N×2 or N×3).
#'
#' For the 2D data, you can fit \code{"euclidean"} (scale, rotation, and translation),
#' \code{"affine"} (+shear), or \code{"projective"} (+tilt) transformations. For 3D data, you can fit
#' \code{"euclidean_x"}, \code{"euclidean_y"}, \code{"euclidean_z"} (scale, rotation around the
#' specified axis, and translation), \code{"euclidean"} (scale, rotation around all three axes, and translation),
#' \code{"affine"} (\code{"euclidean"} + shear for x-plane), and \code{"projective"} (+shear for y and z planes)
#' transformations.
#'
#' For all rotation transformations, priors are uniform distribution within -π..π range.
#'
#' @param iv a data frame containing independent variable, must by numeric only, N×2 or N×3.
#' @param dv a data frame containing dependent variable, must by numeric only, N×2 or N×3.
#' @param transformation the transformation to be used, either \code{'euclidean'}, \code{'affine'}, or \code{'projective'}.
#' @param priors named list of parameters prior distribution. Except for \code{sigma} that uses exponential prior,
#' all other parameters use normal or transformed normal prior and require parameter pairs. E.g.,
#' \code{list("translation" = c(0, 10), "sigma"=1)}.
#' @param chains Number of chains for sampling.
#' @param cores Number of CPU cores to use for sampling. If omitted, all available cores are used.
#' @param ... Additional arguments passed to \code{\link[rstan:sampling]{sampling}} function.
#' @export
#' @seealso \code{\link{fit_geometric_transformation}}
#' @examples
#' \dontrun{
#' # Geometric transformations of 2D data
#' euc2 <- fit_geometric_transformation_df(NakayaData[, 1:2], NakayaData[, 3:4],
#'   'euclidean')
#' euc3x <- fit_geometric_transformation_df(female_face_neutral,
#'   female_face_happy, "euclidean_x")
#' }

fit_geometric_transformation_df <- function(iv, dv, transformation, priors=NULL, chains=4, cores=NULL, ...) {
  if (!is.data.frame(dv) && !is.matrix(dv)) stop("dv must be a data.frame or a matrix")
  if (!is.data.frame(iv) && !is.matrix(iv)) stop("iv must be a data.frame or a matrix")

  tridim <- tridim_transform(transformation, iv, dv, NULL, priors)

  # fitting function
  tridim$stanfit <- rstan::sampling(tridim$stanmodel,
                                    data=tridim$data,
                                    chains=chains,
                                    cores=ifelse(is.null(cores), future::availableCores(), cores),
                                    ...)
  tridim
}
