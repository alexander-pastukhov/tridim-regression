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
  testthat::expect_error(bi_transformation_matrix(euc_params, "affine"))
})


test_that("3D transformation matrix works", {
  # euclidean, single axis rotation
  euc_params <- data.frame(scale.1 = 1,
                           scale.2 = 1,
                           scale.3 = 1,
                           rotation = 0,
                           translation.1 = 0,
                           translation.2 = 0,
                           translation.3 = 0)
  testthat::expect_is(tri_transformation_matrix(euc_params, "euclidean_x"), "matrix")
  testthat::expect_type(tri_transformation_matrix(euc_params, "euclidean_x"), "double")
  testthat::expect_equal(dim(tri_transformation_matrix(euc_params, "euclidean_x")), c(4, 4))
  testthat::expect_is(tri_transformation_matrix(euc_params, "euclidean_y"), "matrix")
  testthat::expect_type(tri_transformation_matrix(euc_params, "euclidean_y"), "double")
  testthat::expect_equal(dim(tri_transformation_matrix(euc_params, "euclidean_y")), c(4, 4))
  testthat::expect_is(tri_transformation_matrix(euc_params, "euclidean_z"), "matrix")
  testthat::expect_type(tri_transformation_matrix(euc_params, "euclidean_z"), "double")
  testthat::expect_equal(dim(tri_transformation_matrix(euc_params, "euclidean_z")), c(4, 4))

  # missing parameters, NULL will cause a matrix size warning
  testthat::expect_error(tri_transformation_matrix(euc_params, "euclidean"))
  euc_params$rotation.1 <- 1
  euc_params$rotation.2 <- 0
  euc_params$rotation.3 <- 0
  testthat::expect_is(tri_transformation_matrix(euc_params, "euclidean"), "matrix")
  testthat::expect_type(tri_transformation_matrix(euc_params, "euclidean"), "double")
  testthat::expect_equal(dim(tri_transformation_matrix(euc_params, "euclidean")), c(4, 4))

  # affine
  affine_params <- euc_params
  affine_params$shearX.1 <- 0
  affine_params$shearX.2 <- 0
  testthat::expect_is(tri_transformation_matrix(affine_params, "affine"), "matrix")
  testthat::expect_type(tri_transformation_matrix(affine_params, "affine"), "double")
  testthat::expect_equal(dim(tri_transformation_matrix(affine_params, "affine")), c(4, 4))
  testthat::expect_error(bi_transformation_matrix(affine_params, "projective"))

  # # projective
  proj_params <- affine_params
  proj_params$shearY.1 <- 0
  proj_params$shearY.2 <- 0
  proj_params$shearZ.1 <- 0
  proj_params$shearZ.2 <- 0
  testthat::expect_is(tri_transformation_matrix(proj_params, "projective"), "matrix")
  testthat::expect_type(tri_transformation_matrix(proj_params, "projective"), "double")
  testthat::expect_equal(dim(tri_transformation_matrix(proj_params, "projective")), c(4, 4))


})
