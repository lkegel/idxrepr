context("paa")

test_that("represent", {
  paa <- mgr_init("paa")
  x <- seq(132)

  result <- mgr_represent(paa, x)

  m <- matrix(x, ncol = 22, nrow = 6, byrow = T)
  expected <- apply(m, 1, mean)

  expect_equal(expected, result)
})

test_that("represent2", {
  paa <- mgr_init("paa")

  expect_error(mgr_set_config(paa, list(n = 1000)))
  expect_error(mgr_set_config(paa, list(w = 10)))

  paa <- mgr_set_config(paa, list(n = 1000, w = 10))
  x <- seq(1000)

  result <- mgr_represent(paa, x)

  m <- matrix(x, ncol = 100, nrow = 10, byrow = T)
  expected <- apply(m, 1, mean)

  expect_equal(expected, result)
})

test_that("distance", {
  paa <- mgr_init("paa")
  x.raw <- seq(132)
  y.raw <- seq(-1, -132)

  x.paa <- mgr_represent(paa, x.raw)
  y.paa <- mgr_represent(paa, y.raw)

  expect_equal(0, mgr_distance(paa, x.paa, x.paa))

  m <- matrix(x.raw, ncol = 22, nrow = 6, byrow = T)
  expected <- sqrt(sum((2 * apply(m, 1, mean))^2)) * sqrt(22)

  expect_equal(expected, mgr_distance(paa, x.paa, y.paa))
})

test_that("distance2", {
  paa <- mgr_init("paa")
  paa <- mgr_set_config(paa, list(n = 1000, w = 10))
  x.raw <- seq(1000)
  y.raw <- seq(-1, -1000)

  x.paa <- mgr_represent(paa, x.raw)
  y.paa <- mgr_represent(paa, y.raw)

  m <- matrix(x.raw, ncol = 100, nrow = 10, byrow = T)
  expected <-  sqrt(sum((2 * apply(m, 1, mean))^2)) * sqrt(100)

  expect_equal(expected, mgr_distance(paa, x.paa, y.paa))
})
