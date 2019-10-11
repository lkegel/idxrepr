context("1d_sax")

test_that("represent_distance", {
  method <- mgr_init("1d_sax")
  expect_equal(length(method), 3)

  # Data
  osl <- list(c(seq(0,  4), seq( 4, 0)),
              c(seq(0, -4), seq(-4, 0)))
  osl <- lapply(osl, function(x) ((x - mean(x)) / sd(x)))

  # Configs
  n <- 10
  w <- 2
  method$`1d_paa`$paa$n <- n
  method$`1d_paa`$paa$w <- w
  method$`1d_paa`$lr$TT <- n / w

  repr <- lapply(osl, represent, method = method)

  di <- distance(method, repr[[1]], repr[[2]])
  expected <- sqrt(sum((osl[[1]] - osl[[2]])^2))
  expect_true(di <= expected)


  # Configs
  method$`1d_paa`$paa$n <- n
  method$`1d_paa`$paa$w <- w
  method$`1d_paa`$lr$TT <- n / w
  method$sax_a <- mgr_set_config(method$sax_a, list(a = 256))
  method$sax_s <- mgr_set_config(method$sax_s, list(a = 256))


  osl <- list(c(seq(0,  4), seq( 4, 0)),
              c(seq(0, -4), seq(-4, 0)))
  osl <- lapply(osl, function(x) ((x - mean(x)) / sd(x)))

  repr <- lapply(osl, represent, method = method)

  di <- distance(method, repr[[1]], repr[[2]])
  expected <- sqrt(sum((osl[[1]] - osl[[2]])^2))
  expect_true(round(di - expected) == 0)

  # AirPassengers
  # n <- 144
  # w <- 72
  # method$`1d_paa`$paa$n <- n
  # method$`1d_paa`$paa$w <- w
  # method$`1d_paa`$lr$TT <- n / w
  # method$sax_a <- mgr_set_config(method$sax_a, list(a = 1024))
  # method$sax_s <- mgr_set_config(method$sax_s, list(a = 1024))
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
  # expect_true(round(di - expected) == 0)
})
