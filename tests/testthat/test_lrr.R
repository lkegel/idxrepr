context("lrr")

test_that("represent", {
  method <- mgr_init("lrr")
  expect_equal(1, length(method))
  expect_equal(132, method$TT)

  expect_error(set_config(method, list()))
  method <- set_config(method, list(TT = 200))
  expect_equal(200, method$TT)

  series <- seq(200) - mean(seq(200))
  x <- represent(method, series)
  expect_equal(45 * pi / 180, x)

  expect_equal(to_series(method, x), series)

  series_2 <- 2 * seq(200) - mean(2 * seq(200))
  y <- represent(method, series_2)
  expect_equal(distance(method, x, y), sqrt(sum((series - series_2)**2)))
})
