context("lrrsax")

test_that("represent", {
  method <- mgr_init("lrrsax")
  method <- mgr_set_config(method, list(a = 4))
  expect_equal(length(method), 7)

  osl <- list(seq(method$lr$TT), seq(-1, -method$lr$TT) + 100)
  osl <- lapply(osl, function(x) (x - mean(x)) / sd(x))
  repr <- lapply(osl, represent.lrrsax, method = method)

  expect_equal(repr[[1]], 3)
  expect_equal(repr[[2]], 0)
})

test_that("distance", {
  method <- mgr_init("lrrsax")
  method <- mgr_set_config(method, list(a = 4))

  # No neighborhood
  x <- 0
  y <- 3
  x_raw <- to_series(method$lrr, method$qnorm[2])
  y_raw <- to_series(method$lrr, method$qnorm[4])
  expected <- distance.ed(NULL, x_raw, y_raw)
  expect_equal(expected, distance_q.lrrsax(method, x, y))
  expect_equal(expected, distance_lut.lrrsax(method, x, y))

  # Neighborhood
  expect_equal(distance_q.lrrsax(method, 0, 1), 0)
  expect_equal(distance_q.lrrsax(method, 0, 1), 0)
  expect_equal(distance_lut.lrrsax(method, 2, 2), 0)
  expect_equal(distance_lut.lrrsax(method, 2, 2), 0)
})
