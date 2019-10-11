context("seassaxres")

test_that("unaggregated", {
  method <- mgr_init("seassaxres")
  expect_equal(length(method), 3)
  method$sax$paa$w <- 11

  series <- rep(seq(12), 11) + rnorm(132, sd = 0.01)
  series <- (series - mean(series)) / sd(series)
  repr <- mgr_represent(method, series)

  expected <- c(rep(0, 5), rep(1, 2), rep(2, 5), rep(1, 11))
  expect_equal(expected, round(repr))

  series_2 <- rep(seq(-1, -12), 11) + rnorm(132, sd = 0.01)
  series_2 <- (series_2 - mean(series_2)) / sd(series_2)
  repr_2 <- mgr_represent(method, series_2)

  expected_2 <- c(rep(2, 5), rep(1, 2), rep(0, 5), rep(1, 11))
  expect_equal(expected_2, round(repr_2))

  expect_equal(distance(method, repr, repr_2), 0)
  expect_equal(distance.seassaxres(method, repr, repr_2), 0)

  method$seassax$sax <- set_config(method$seassax$sax, list(a = 2**8))
  method$sax <- set_config(method$sax, list(a = 2**8))
  method <- set_config(method, list())
  repr <- mgr_represent(method, series)
  repr_2 <- mgr_represent(method, series_2)
  expected <- sqrt(sum((series - series_2)^2))
  expect_true(abs(distance(method, repr, repr_2) - expected) < 1)
  expect_true((distance(method, repr, repr_2) -
                 distance_q.seassaxres(method, repr, repr_2)) < 0.0001)
})


test_that("w = 6", {
  method <- mgr_init("seassaxres")
  expect_equal(length(method), 3)
  method$seas$TT <- 144
  method$seassax$seas$TT <- 144
  method$seassax$sax$paa$n <- 144
  method$seassax$sax$paa$w <- 6
  method$sax$paa$n <- 144
  method$sax$paa$w <- 6

  series <- rep(seq(12), 12) + rnorm(144, sd = 0.01)
  series <- (series - mean(series)) / sd(series)
  repr <- mgr_represent(method, series)

  expected <- c(rep(0, 5), rep(1, 2), rep(2, 5), rep(1, 6))
  expect_equal(expected, round(repr))

  series_2 <- rep(seq(-1, -12), 12) + rnorm(144, sd = 0.01)
  series_2 <- (series_2 - mean(series_2)) / sd(series_2)
  repr_2 <- mgr_represent(method, series_2)

  expected_2 <- c(rep(2, 5), rep(1, 2), rep(0, 5), rep(1, 6))
  expect_equal(expected_2, round(repr_2))

  expect_equal(distance(method, repr, repr_2), 0)

  method$seassax$sax <- set_config(method$seassax$sax, list(a = 2**8))
  method$sax <- set_config(method$sax, list(a = 2**8))
  method <- set_config(method, list())
  repr <- mgr_represent(method, series)
  repr_2 <- mgr_represent(method, series_2)
  expected <- sqrt(sum((series - series_2)^2))
  expect_true(abs(distance(method, repr, repr_2) - expected) < 1)
})

test_that("w = 24", {
  method <- mgr_init("seassaxres")
  expect_equal(length(method), 3)
  method$seas$TT <- 144
  method$seassax$seas$TT <- 144
  method$seassax$sax$paa$n <- 144
  method$seassax$sax$paa$w <- 24
  method$sax$paa$n <- 144
  method$sax$paa$w <- 24

  series <- rep(seq(12), 12) + rnorm(144, sd = 0.01)
  series <- (series - mean(series)) / sd(series)
  repr <- mgr_represent(method, series)

  expected <- c(rep(0, 5), rep(1, 2), rep(2, 5), rep(1, 24))
  expect_equal(expected, round(repr))

  series_2 <- rep(seq(-1, -12), 12) + rnorm(144, sd = 0.01)
  series_2 <- (series_2 - mean(series_2)) / sd(series_2)
  repr_2 <- mgr_represent(method, series_2)

  expected_2 <- c(rep(2, 5), rep(1, 2), rep(0, 5), rep(1, 24))
  expect_equal(expected_2, round(repr_2))

  expect_equal(distance(method, repr, repr_2), 0)

  method$seassax$sax <- set_config(method$seassax$sax, list(a = 2**8))
  method$sax <- set_config(method$sax, list(a = 2**8))
  method <- set_config(method, list())
  repr <- mgr_represent(method, series)
  repr_2 <- mgr_represent(method, series_2)
  expected <- sqrt(sum((series - series_2)^2))
  expect_true(abs(distance(method, repr, repr_2) - expected) < 1)
})

test_that("distance_lut", {
  method <- mgr_init("seassaxres")

  method$sax <- set_config(method$sax, list(a = 2**4))
  method$seassax$sax <- set_config(method$seassax$sax, list(a = 2**4))
  method <- set_config(method, list())

  TT <- 132
  B <- 11
  L <- 12
  qres <- method$sax$qnorm
  qseas <- method$seassax$sax$qnorm

  mres <- method$sax$lut
  mseas <- method$seassax$sax$lut

  for (i in seq(0, 8)) {
    for (j in seq(0, 8)) {
      x <- rep(i, 132)
      y <- rep(j, 132)
      expect_equal(d_seassaxres(x, y, B, L, 1, qres, qseas),
                   d_seassaxres_lut(x, y, TT, B, L, mres, mseas))
    }
  }
})
