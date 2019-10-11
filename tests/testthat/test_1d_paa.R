context("1d_paa")

test_that("represent_distance", {
  method <- mgr_init("1d_paa")
  expect_equal(length(method), 2)

  n <- 10
  w <- 2
  method$paa$n <- n
  method$paa$w <- w
  method$lr$TT <- n / w

  osl <- list(c(seq(0,  4), seq( 4, 0)),
              c(seq(0, -4), seq(-4, 0)))
  repr <- lapply(osl, represent, method = method)

  expected <- c(2.5, 1, 1.5, -1)
  expect_true(all(round(repr[[1]] - expected, 6) == 0))
  expect_true(all(round(repr[[2]] + expected, 6) == 0))

  dist <- distance(method, repr[[1]], repr[[2]])
  expected <- sqrt(sum(2 * (2 * c(1, 2, 3, 4))^2))
  expect_true(round(dist - expected, 6) == 0)

  # Visualization
  # n <- 144
  # w <- 24
  # method$paa$n <- n
  # method$paa$w <- w
  # method$lr$TT <- n / w
  #
  # repr <- represent(method, as.numeric(AirPassengers))
  #
  # plot(to_series(method, repr))

  # AirPassengers
  # n <- 144
  # w <- 72
  # method$paa$n <- n
  # method$paa$w <- w
  # method$lr$TT <- n / w
  #
  # repr <- represent(method, as.numeric(AirPassengers))
  # osl <- list(as.numeric(AirPassengers),
  #             as.numeric(AirPassengers + rnorm(n, sd = 10)))
  # osl <- lapply(osl, function(x) ((x - mean(x)) / sd(x)))
  #
  # repr <- lapply(osl, represent, method = method)
  #
  # di <- distance(method, repr[[1]], repr[[2]])
  # expected <- sqrt(sum((osl[[1]] - osl[[2]])^2))
})
