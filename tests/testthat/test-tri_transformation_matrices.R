test_that("multiplication works", {
  testthat::expect_is(tri_rotation_x_matrix(1), "matrix")
  testthat::expect_type(tri_rotation_x_matrix(1), "double")
  testthat::expect_equal(dim(tri_rotation_x_matrix(1)), c(4,4))
  testthat::expect_error(tri_rotation_x_matrix("1"))
  testthat::expect_error(tri_rotation_x_matrix(NA))
  testthat::expect_error(tri_rotation_x_matrix(NULL))

  testthat::expect_is(tri_rotation_y_matrix(1), "matrix")
  testthat::expect_type(tri_rotation_y_matrix(1), "double")
  testthat::expect_equal(dim(tri_rotation_y_matrix(1)), c(4,4))
  testthat::expect_error(tri_rotation_y_matrix("1"))
  testthat::expect_error(tri_rotation_y_matrix(NA))
  testthat::expect_error(tri_rotation_y_matrix(NULL))

  testthat::expect_is(tri_rotation_z_matrix(1), "matrix")
  testthat::expect_type(tri_rotation_z_matrix(1), "double")
  testthat::expect_equal(dim(tri_rotation_z_matrix(1)), c(4,4))
  testthat::expect_error(tri_rotation_z_matrix("1"))
  testthat::expect_error(tri_rotation_z_matrix(NA))
  testthat::expect_error(tri_rotation_z_matrix(NULL))

  testthat::expect_is(tri_shear_x_matrix(0, 1), "matrix")
  testthat::expect_type(tri_shear_x_matrix(0, 1), "double")
  testthat::expect_equal(dim(tri_shear_x_matrix(0, 1)), c(4,4))

  testthat::expect_is(tri_shear_y_matrix(0, 1), "matrix")
  testthat::expect_type(tri_shear_y_matrix(0, 1), "double")
  testthat::expect_equal(dim(tri_shear_y_matrix(0, 1)), c(4,4))

  testthat::expect_is(tri_shear_z_matrix(0, 1), "matrix")
  testthat::expect_type(tri_shear_z_matrix(0, 1), "double")
  testthat::expect_equal(dim(tri_shear_z_matrix(0, 1)), c(4,4))

  valid <- c(0, 1)
  bad_values <- c("1", NA)
  testthat::expect_is(tri_shear_x_matrix(valid[1], valid[2]), "matrix")
  for(bad_value in bad_values) {
    for(iN in 1:length(valid)){

      invalid <- valid
      invalid[iN] <- bad_value
      testthat::expect_error(tri_shear_x_matrix(invalid[1], invalid[2]))
      testthat::expect_error(tri_shear_y_matrix(invalid[1], invalid[2]))
      testthat::expect_error(tri_shear_z_matrix(invalid[1], invalid[2]))
    }
  }

  testthat::expect_error(tri_shear_x_matrix(1, NULL))
  testthat::expect_error(tri_shear_y_matrix(1, NULL))
  testthat::expect_error(tri_shear_z_matrix(1, NULL))
  testthat::expect_error(tri_shear_x_matrix(NULL, 1))
  testthat::expect_error(tri_shear_y_matrix(NULL, 1))
  testthat::expect_error(tri_shear_z_matrix(NULL, 1))


  testthat::expect_is(tri_translation_matrix(0, 1, 2), "matrix")
  testthat::expect_type(tri_translation_matrix(0, 1, 2), "double")
  testthat::expect_equal(dim(tri_translation_matrix(0, 1, 2)), c(4,4))

  testthat::expect_is(tri_scale_matrix(0, 1, 2), "matrix")
  testthat::expect_type(tri_scale_matrix(0, 1, 2), "double")
  testthat::expect_equal(dim(tri_scale_matrix(0, 1, 2)), c(4,4))

  valid <- c(0, 1, 2)
  bad_values <- c("1", NA)
  testthat::expect_is(tri_translation_matrix(valid[1], valid[2], valid[3]), "matrix")
  for(bad_value in bad_values) {
    for(iN in 1:length(valid)){

      invalid <- valid
      invalid[iN] <- bad_value
      testthat::expect_error(tri_translation_matrix(invalid[1], invalid[2], invalid[3]))
      testthat::expect_error(tri_scale_matrix(invalid[1], invalid[2], invalid[3]))
    }
  }
  testthat::expect_error(tri_translation_matrix(NULL, 1, 2))
  testthat::expect_error(tri_translation_matrix(1, NULL, 2))
  testthat::expect_error(tri_translation_matrix(1, 2, NULL))

  testthat::expect_error(tri_scale_matrix(NULL, 1, 2))
  testthat::expect_error(tri_scale_matrix(1, NULL, 2))
  testthat::expect_error(tri_scale_matrix(1, 2, NULL))
})
