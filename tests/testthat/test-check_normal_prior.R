context("Priors")
test_that("Priors check", {
  # This should produce no error messages
  expect_equal(check_normal_prior(c(0, 2), "scale"), expected=NULL)

  # Error: non-numeric data
  expect_error(check_normal_prior(c("0", "2"), "scale"))

  # Error: wrong number of values
  expect_error(check_normal_prior(c(0, 2, 3), "scale"))
  expect_error(check_normal_prior(c(0), "scale"))

  # Error: non-positive variance
  expect_error(check_normal_prior(c(0, -2), "scale"))
  expect_error(check_normal_prior(c(0, 0), "scale"))
})
