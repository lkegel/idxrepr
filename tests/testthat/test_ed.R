context("ed")

test_that("ed", {
  method <- mgr_init("ed")

  x <- c(-2, -1, 0, 1, 2)
  y <- c(-1,  1, 1, 1, -1)

  expect_equal(mgr_represent(method, x), x)
  expect_equal(mgr_distance(method, x, y), sqrt(15))
})
