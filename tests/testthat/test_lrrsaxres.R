context("lrrsaxres")

test_that("represent", {
  method <- mgr_init("lrrsaxres")
  expect_equal(2, length(method))

  tr <- seq(0, 131)
  tr <- (tr - mean(tr)) / sd(tr)
  rnvl <- list(tr, -tr)
  repr <- lapply(rnvl, mgr_represent, method = method)

  expected <- rep(1, 7)
  expected[1] <- 2
  expect_equal(repr[[1]], expected)

  expected[1] <- 0
  expect_equal(repr[[2]], expected)

  expect_equal(distance_lut.lrrsaxres(method, repr[[1]], repr[[2]]),
               distance_q.lrrsaxres(method, repr[[1]], repr[[2]]))

  method$lrrsax <- set_config(method$lrrsax, list(a = 2**8))
  method$sax <- set_config(method$sax, list(a = 2**8))
  repr <- lapply(rnvl, mgr_represent, method = method)
  di <- distance(method, repr[[1]], repr[[2]])
  di2 <- distance2.lrrsaxres(method, repr[[1]], repr[[2]])
  expect_true(abs(di - sqrt(sum((rnvl[[1]] - rnvl[[2]])**2))) < 0.5)

  tr_2 <- seq(0, 131) + rnorm(132, sd = 1)
  tr_2 <- (tr_2 - mean(tr_2)) / sd(tr_2)
  tr_3 <- -seq(0, 131) + rnorm(132, sd = 1)
  tr_3 <- (tr_3 - mean(tr_3)) / sd(tr_3)
  rnvl <- list(tr_2, tr_3)
  method$lrrsax <- set_config(method$lrrsax, list(a = 2**8))
  method$sax <- set_config(method$sax, list(a = 2**8))
  repr <- lapply(rnvl, mgr_represent, method = method)
  di <- distance.lrrsaxres(method, repr[[1]], repr[[2]])
  expect_true(abs(di - sqrt(sum((rnvl[[1]] - rnvl[[2]])**2))) < 0.5)
})
