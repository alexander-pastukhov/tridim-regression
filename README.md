# TriDimRegression

<!-- badges: start -->
[![DOI](https://zenodo.org/badge/139134694.svg)](https://zenodo.org/badge/latestdoi/139134694)
[![CRAN status](https://www.r-pkg.org/badges/version/TriDimRegression)](https://cran.r-project.org/package=TriDimRegression)
 <!-- badges: end -->
 
Package to calculate the bidimensional and tridimensional regression between two 2D/3D configurations.

## Installation
From CRAN

```{r}
install.packages("TriDimRegression")
```

From Github
```
library("devtools");
install_github("alexander-pastukhov/tridim-regression", dependencies=TRUE)
```

If you want vignettes, use
```
devtools::install_github("alexander-pastukhov/tridim-regression",
                         dependencies=TRUE,
                         build_vignettes = TRUE)
```

## Using TriDimRegression

You can call the main function either via a formula that specifies dependent and independent variables with the `data` table or by supplying two tables one containing all independent variables and one containing all dependent variables. The former call is
```
euc2 <- fit_transformation(depV1 + depV2 ~ indepV1 + indepV2, NakayaData, 'euclidean')
```
whereas the latter is 
```
euc3 <- fit_transformation_df(Face3D_W070, Face3D_W097, transformation ='translation')
```

See also `vignette("calibration", package="TriDimRegression")` for an example of using `TriDimRegression` for 2D eye gaze data and `vignette("comparing_faces", package="TriDimRegression")` for an example of working with 3D facial landmarks data.

For the 2D data, you can fit `"translation"` (2 parameters for translation only), `"euclidean"`
(4 parameters: 2 for translation, 1 for scaling, and 1 for rotation), `"affine"` (6 parameters: 2 for translation and 4 that jointly describe scaling, rotation and sheer), or `"projective"` (8 parameters: affine plus 2 additional parameters to account for projection). For 3D data, you can fit `"translation"` (3 for translation only), `"euclidean_x"`, `"euclidean_y"`, `"euclidean_z"` (5 parameters: 3 for translation scale, 1 for rotation, and 1 for scaling), `"affine"` (12 parameters: 3 for translation and 9 to account for scaling, rotation, and sheer), and `"projective"` (15 parameters: affine plus 3 additional parameters to account for projection). transformations. For details on how matrices are constructed, see `vignette("transformation_matrices", package="TriDimRegression")`.

Once the data is fitted, you can extract the transformation coefficients via `coef()` function and the matrix itself via `transformation_matrix()`. Predicted data, either based on the original data or on the new data, can be generated via `predict()`. Bayesian R-squared can be computed with or without adjustment via `R2()` function. In all three cases, you have choice between summary (mean + specified quantiles) or full posterior samples. `loo()` and `waic()` provide corresponding measures that can be used for comparison via `loo::loo_compare()` function.

## References

-   Tobler, W. R. (1965). Computation of the corresponding of geographical patterns. Papers of the Regional Science Association, 15, 131-139.
-   Tobler, W. R. (1966). Medieval distortions: Projections of ancient maps. Annals of the Association of American Geographers, 56(2), 351-360.
-   Tobler, W. R. (1994). Bidimensional regression. Geographical Analysis, 26(3), 187-212.
-   Friedman, A., & Kohler, B. (2003). Bidimensional regression: Assessing the configural similarity and accuracy of cognitive maps and other two-dimensional data sets. Psychological Methods, 8(4), 468-491.
-   Nakaya, T. (1997). Statistical inferences in bidimensional regression models. Geographical Analysis, 29(2), 169-186.
-   Waterman, S., & Gordon, D. (1984). A quantitative-comparative approach to analysis of distortion in mental maps. Professional Geographer, 36(3), 326-337.

## License

All code is licensed under the [GPL 3.0](https://opensource.org/licenses/GPL-3.0) license.
