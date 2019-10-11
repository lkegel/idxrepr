context("lr")

test_that("represent", {
  method <- mgr_init("lr")
  expect_equal(1, length(method))
  expect_equal(132, method$TT)

  expect_error(set_config.lr(method, list()))
  method <- set_config.lr(method, list(TT = 200))
  expect_equal(200, method$TT)

  x <- represent.lr(method, seq(200))
  expect_equal(c(1), round(x[1], 6))
  expect_equal(c(1), x[2])

  expect_equal(seq(200), to_series.lr(method, x))

  y <- represent.lr(method, seq(200) - 10)
  expect_equal(sqrt(200 * 10 ^ 2), distance.lr(method, x, y))
})
