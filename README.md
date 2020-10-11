# TriDimRegression

Package to calculate the bidimensional and tridimensional regression between two 3D configurations.

## Installation from Github
```
library("devtools"); install_github("alexander-pastukhov/tridim-regression",dependencies=TRUE)
```
## Using TriDimRegression

You can call the main function `fit_geometric_transformation()` either via a formula that specifies dependent and independent variables with the `data` table or by supplying two tables one containing all independent variables and one containing all dependent variables. The former call is
```
euc2 <- fit_geometric_transformation(depV1 + depV2 ~ indepV1 + indepV2, NakayaData, 'euclidean')
```
whereas the latter is 
```
euc3 <- fit_geometric_transformation(female_face_neutral, female_face_neutral, 'euclidean')
```


For the 2D data, you can fit `"euclidean"` (scale, rotation, and translation), `"affine"` (+shear), or `"projective"` (+tilt). For 3D data, you can fit `"euclidean_x"`, `"euclidean_y"`, `"euclidean_z"` (scale, rotation around the specified axis, and translation), `"euclidean"` (scale, rotation around all three axes, and translation), `"affine"` (`"euclidean"` + shear for x-plane), and `"projective"` (+shear for y and z planes).

Once the data is fitted, you can extract the transformation coefficients via `coef()` function. Predicted data, either based on the original data or on the new data, can be generated via `predict()`. Posterior R-squared can be computed with or without adjustment via `R2()` funciton. In all three cases, you have choice between summary (mean + specified quantiles) or full posterior samples. `loo()` and `waic()` provide corresponding measures that can be used for comparison via `loo::loo_compare()` function.

## References

-   Tobler, W. R. (1965). Computation of the corresponding of geographical patterns. Papers of the Regional Science Association, 15, 131-139.
-   Tobler, W. R. (1966). Medieval distortions: Projections of ancient maps. Annals of the Association of American Geographers, 56(2), 351-360.
-   Tobler, W. R. (1994). Bidimensional regression. Geographical Analysis, 26(3), 187-212.
-   Friedman, A., & Kohler, B. (2003). Bidimensional regression: Assessing the configural similarity and accuracy of cognitive maps and other two-dimensional data sets. Psychological Methods, 8(4), 468-491.
-   Nakaya, T. (1997). Statistical inferences in bidimensional regression models. Geographical Analysis, 29(2), 169-186.
-   Waterman, S., & Gordon, D. (1984). A quantitative-comparative approach to analysis of distortion in mental maps. Professional Geographer, 36(3), 326-337.

## License

All code is licensed under the [GPL 3.0](https://opensource.org/licenses/GPL-3.0) license.
