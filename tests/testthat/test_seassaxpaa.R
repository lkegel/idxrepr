context("seassaxpaa")

test_that("unaggregated", {
  method <- mgr_init("seassaxpaa")
  expect_equal(length(method), 3)
  method$paa$w <- 11

  series <- rep(seq(12), 11) + rnorm(132, sd = 0.01)
  series <- (series - mean(series)) / sd(series)
  repr <- mgr_represent(method, series)

  expected <- c(rep(0, 5), rep(1, 2), rep(2, 5), rep(0, 11))
  expect_equal(expected, round(repr))

  series_2 <- rep(seq(-1, -12), 11) + rnorm(132, sd = 0.01)
  series_2 <- (series_2 - mean(series_2)) / sd(series_2)
  repr_2 <- mgr_represent(method, series_2)

  expected_2 <- c(rep(2, 5), rep(1, 2), rep(0, 5), rep(0, 11))
  expect_equal(expected_2, round(repr_2))

  expected <- sqrt(sum(10 * (2 * qnorm(2/3))^2)) * sqrt(11)
  expect_true(abs(distance(method, repr, repr_2) - expected) < 0.1)
})

test_that("w = 12", {
  method <- mgr_init("seassaxpaa")
  expect_equal(length(method), 3)
  method$seas$TT <- 144
  method$seassax$seas$TT <- 144
  method$seassax$sax$paa$n <- 144
  method$seassax$sax$paa$w <- 6
  method$paa$w <- 6
  method$paa$n <- 144

  series <- rep(seq(12), 12) + rnorm(144, sd = 0.01)
  series <- (series - mean(series)) / sd(series)
  repr <- mgr_represent(method, series)

  expected <- c(rep(0, 5), rep(1, 2), rep(2, 5), rep(0, 6))
  expect_equal(expected, round(repr))

  series_2 <- rep(seq(-1, -12), 12) + rnorm(144, sd = 0.01)
  series_2 <- (series_2 - mean(series_2)) / sd(series_2)
  repr_2 <- mgr_represent(method, series_2)

  expected_2 <- c(rep(2, 5), rep(1, 2), rep(0, 5), rep(0, 6))
  expect_equal(expected_2, round(repr_2))

  expected <- sqrt(sum(10 * (2 * qnorm(2/3))^2)) * sqrt(12)
  expect_true(abs(distance(method, repr, repr_2) - expected) < 0.1)
})
