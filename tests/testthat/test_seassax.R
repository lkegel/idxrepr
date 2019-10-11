context("seassax")

test_that("unaggregated", {
  method <- mgr_init("seassax")
  expect_equal(2, length(method))

  series <- rep(seq(12), 11)
  series <- (series - mean(series)) / sd(series)
  repr <- mgr_represent(method, series)

  expected <- c(rep(0, 5), rep(1, 2), rep(2, 5))
  expect_equal(expected, repr)

  series_2 <- rep(seq(-1, -12), 11)
  series_2 <- (series_2 - mean(series_2)) / sd(series_2)
  repr_2 <- mgr_represent(method, series_2)

  expected_2 <- c(rep(2, 5), rep(1, 2), rep(0, 5))
  expect_equal(expected_2, repr_2)

  expected <- sqrt(sum(10 * (2 * qnorm(2/3))^2)) * sqrt(11)
  expect_equal(expected, mgr_distance(method, repr, repr_2))
})

test_that("aggregated", {
  method <- mgr_init("seassax")
  method$seas$w <- 3

  series <- rep(seq(12), 11)
  series <- (series - mean(series)) / sd(series)
  repr <- mgr_represent(method, series)

  expected <- c(0, 1, 2)
  expect_equal(expected, repr)

  series_2 <- rep(seq(-1, -12), 11)
  series_2 <- (series_2 - mean(series_2)) / sd(series_2)
  repr_2 <- mgr_represent(method, series_2)

  expected_2 <- c(2, 1, 0)
  expect_equal(expected_2, repr_2)

  expected <- sqrt((2 * qnorm(2/3))^2 * 2) * sqrt(44)
  expect_equal(expected, mgr_distance(method, repr, repr_2))
})
