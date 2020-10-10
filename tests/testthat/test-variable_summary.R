test_that("variable summary works", {
  expect_is(variable_summary("test", matrix(1:1000, ncol = 1), c(0.05, 0.95)), "data.frame")
  expect_equal(nrow(variable_summary("test", matrix(1:1000, ncol = 1), c(0.05, 0.95))), 1)
  expect_equal(ncol(variable_summary("test", matrix(1:1000, ncol = 1), c(0.05, 0.95))), 3)
  expect_equal(ncol(variable_summary("test", matrix(1:1000, ncol = 1), 0.5)), 2)
  expect_equal(ncol(variable_summary("test", matrix(1:1000, ncol = 1), NULL)), 1)

  # valid coefficient name
  expect_error(variable_summary(NULL, c(1, 1, 1), NULL))
  expect_error(variable_summary(NA, c(1, 1, 1), NULL))

  # valid probs
  expect_error(variable_summary("test", c(1, 1, 1), NA))
  expect_error(variable_summary("test", c(1, 1, 1), "0.05"))
  expect_error(variable_summary("test", c(1, 1, 1), factor(0.05)))


  # invalid matrix
  expect_error(variable_summary("test", c(1, "NA", 1), c(0.05, 0.99)))
})

