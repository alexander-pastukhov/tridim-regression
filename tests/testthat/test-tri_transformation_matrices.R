test_that("multiplication works", {
  testthat::expect_is(tri_rotation_x_matrix(1), "matrix")
  testthat::expect_type(tri_rotation_x_matrix(1), "double")
  testthat::expect_equal(dim(tri_rotation_x_matrix(1)), c(4,4))

  testthat::expect_is(tri_rotation_y_matrix(1), "matrix")
  testthat::expect_type(tri_rotation_y_matrix(1), "double")
  testthat::expect_equal(dim(tri_rotation_y_matrix(1)), c(4,4))

  testthat::expect_is(tri_rotation_z_matrix(1), "matrix")
  testthat::expect_type(tri_rotation_z_matrix(1), "double")
  testthat::expect_equal(dim(tri_rotation_z_matrix(1)), c(4,4))

  testthat::expect_is(tri_shear_x_matrix(0, 1), "matrix")
  testthat::expect_type(tri_shear_x_matrix(0, 1), "double")
  testthat::expect_equal(dim(tri_shear_x_matrix(0, 1)), c(4,4))

  testthat::expect_is(tri_shear_y_matrix(0, 1), "matrix")
  testthat::expect_type(tri_shear_y_matrix(0, 1), "double")
  testthat::expect_equal(dim(tri_shear_y_matrix(0, 1)), c(4,4))

  testthat::expect_is(tri_shear_z_matrix(0, 1), "matrix")
  testthat::expect_type(tri_shear_z_matrix(0, 1), "double")
  testthat::expect_equal(dim(tri_shear_z_matrix(0, 1)), c(4,4))

  testthat::expect_is(tri_translation_matrix(0, 1, 2), "matrix")
  testthat::expect_type(tri_translation_matrix(0, 1, 2), "double")
  testthat::expect_equal(dim(tri_translation_matrix(0, 1, 2)), c(4,4))

  testthat::expect_is(tri_scale_matrix(0, 1, 2), "matrix")
  testthat::expect_type(tri_scale_matrix(0, 1, 2), "double")
  testthat::expect_equal(dim(tri_scale_matrix(0, 1, 2)), c(4,4))
})
