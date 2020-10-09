test_that("2D transformation matrix works", {
  # euclidean
  euc_params <- data.frame(scale.1 = 1,
                           scale.2 = 1,
                           rotation = 0,
                           translation.1 = 0,
                           translation.2 = 0)
  testthat::expect_is(bi_transformation_matrix(euc_params, "euclidean"), "matrix")
  testthat::expect_type(bi_transformation_matrix(euc_params, "euclidean"), "double")
  testthat::expect_equal(dim(bi_transformation_matrix(euc_params, "euclidean")), c(3, 3))

  # affine
  affine_params <- euc_params
  affine_params$shear <- 0
  testthat::expect_is(bi_transformation_matrix(affine_params, "affine"), "matrix")
  testthat::expect_type(bi_transformation_matrix(affine_params, "affine"), "double")
  testthat::expect_equal(dim(bi_transformation_matrix(affine_params, "affine")), c(3, 3))

  # projective
  proj_params <- affine_params
  proj_params$tilt.1 <- 0
  proj_params$tilt.2 <- 0
  testthat::expect_is(bi_transformation_matrix(proj_params, "projective"), "matrix")
  testthat::expect_type(bi_transformation_matrix(proj_params, "projective"), "double")
  testthat::expect_equal(dim(bi_transformation_matrix(proj_params, "projective")), c(3, 3))

  # missing parameters, NULL will cause a matrix size warning
  testthat::expect_warning(bi_transformation_matrix(euc_params, "affine"))
})
