context("Function inputs")
test_that("Variables check", {

  # This should produce no error messages
  expect_equal(check_variables(matrix(c(1, 2, 3, 4), ncol=2), "test matrix"), expected=NULL)

  # This should produce non-numeric data error message
  expect_error(check_variables(matrix(as.character(c(1, 2, 3, 4)), ncol=2), "test matrix"))

  # These should produce non-finite data error message
  expect_error(check_variables(matrix(c(1, 2, NA, 4), ncol=2), "test matrix"))
  expect_error(check_variables(matrix(c(1, 2, Inf, 4), ncol=2), "test matrix"))
})
