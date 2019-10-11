context("lrrsaxpaa")

test_that("represent", {
  method <- mgr_init("lrrsaxpaa")
  expect_equal(2, length(method))

  tr <- seq(0, 131)
  tr <- (tr - mean(tr)) / sd(tr)
  rnvl <- list(tr, -tr)
  repr <- lapply(rnvl, mgr_represent, method = method)

  expected <- rep(0, 7)
  expected[1] <- 2
  expect_equal(round(repr[[1]], 6), expected)

  expected[1] <- 0
  expect_equal(round(repr[[2]], 6), expected)

  di <- distance(method, repr[[1]], repr[[2]])
  x <- to_series.lrr(method$lrrsax$lrr, method$lrrsax$qnorm[1])
  y <- to_series.lrr(method$lrrsax$lrr, method$lrrsax$qnorm[2])
  expect_true(round(di - sqrt(sum((x - y)^2)), 8) < 0.1)

  method$lrrsax <- set_config(method$lrrsax, list(a = 2**8))
  repr <- lapply(rnvl, mgr_represent, method = method)
  di <- distance(method, repr[[1]], repr[[2]])
  expect_true(di / sqrt(sum((rnvl[[1]] - rnvl[[2]])**2)) > 0.99)

  tr_2 <- seq(0, 131) + rnorm(132, sd = 20)
  tr_2 <- (tr_2 - mean(tr_2)) / sd(tr_2)
  tr_3 <- -seq(0, 131) + rnorm(132, sd = 20)
  tr_3 <- (tr_3 - mean(tr_3)) / sd(tr_3)
  rnvl <- list(tr_2, tr_3)
  method$lrrsax <- set_config(method$lrrsax, list(a = 2**4))
  repr <- lapply(rnvl, mgr_represent, method = method)
  di <- distance.lrrsaxpaa(method, repr[[1]], repr[[2]])
  expect_true(di - sqrt(sum((rnvl[[1]] - rnvl[[2]])**2)) < 0.001)
})
