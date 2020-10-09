test_that("bi-matrices work", {
  testthat::expect_is(bi_rotation_matrix(1), "matrix")
  testthat::expect_type(bi_rotation_matrix(1), "double")
  testthat::expect_equal(dim(bi_rotation_matrix(1)), c(3, 3))

  testthat::expect_is(bi_translation_matrix(2, 2), "matrix")
  testthat::expect_type(bi_translation_matrix(2, 3), "double")
  testthat::expect_equal(dim(bi_translation_matrix(2, 3)), c(3, 3))

  testthat::expect_is(bi_scale_matrix(0, 1), "matrix")
  testthat::expect_type(bi_scale_matrix(2, 3), "double")
  testthat::expect_equal(dim(bi_scale_matrix(2, 3)), c(3, 3))

  testthat::expect_is(bi_shear_matrix(0, 1), "matrix")
  testthat::expect_type(bi_shear_matrix(0, 3), "double")
  testthat::expect_equal(dim(bi_shear_matrix(0, 3)), c(3, 3))

  testthat::expect_is(bi_tilt_matrix(0, 1), "matrix")
  testthat::expect_type(bi_tilt_matrix(0, 3), "double")
  testthat::expect_equal(dim(bi_tilt_matrix(0, 3)), c(3, 3))
})
