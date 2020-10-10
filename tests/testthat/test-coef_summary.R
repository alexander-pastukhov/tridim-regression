test_that("coef_summary works", {
  expect_is(coef_summary("test", c(1, 1, 1), NULL), "data.frame")
  expect_is(coef_summary("test", matrix(1:100, ncol=2), c(0.05, 0.095)), "data.frame")
  expect_equal(nrow(coef_summary("test", matrix(1:100, ncol=1), NULL)), 1)
  expect_equal(nrow(coef_summary("test", matrix(1:100, ncol=2), NULL)), 2)
  expect_equal(ncol(coef_summary("test", matrix(1:100, ncol=2), NULL)), 2)
  expect_equal(ncol(coef_summary("test", matrix(1:100, ncol=2), 0.05)), 3)
  expect_equal(ncol(coef_summary("test", matrix(1:100, ncol=2), c(0.05, 0.095))), 4)

  # valid coefficient name
  expect_error(coef_summary(NULL, c(1, 1, 1), NULL))
  expect_error(coef_summary(NA, c(1, 1, 1), NULL))

  # valid probs
  expect_error(coef_summary("test", c(1, 1, 1), NA))
  expect_error(coef_summary("test", c(1, 1, 1), "0.05"))
  expect_error(coef_summary("test", c(1, 1, 1), factor(0.05)))


  # invalid matrix
  expect_error(coef_summary("test", c(1, "NA", 1), c(0.05, 0.99)))
})



coef_summary("test", c(1, 1, 1), NULL)
