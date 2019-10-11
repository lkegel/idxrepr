context("lrrpaa")

test_that("represent", {
  method <- mgr_init("lrrpaa")
  expect_equal(length(method), 2)

  osl <- list(seq(method$lrr$TT), seq(-1, -method$lrr$TT) + 100)
  osl <- lapply(osl, function(x) (x - mean(x)) / sd(x))
  repr <- lapply(osl, represent, method = method)

  phi_1 <- atan(-2 * osl[[1]][1] / (method$lrr$TT - 1))
  expected <- c(phi_1, rep(0, method$paa$w))
  expect_true(all(round(repr[[1]] - expected, 6) == 0))

  phi_1 <- atan(-2 * osl[[2]][1] / (method$lrr$TT - 1))
  expected <- c(phi_1, rep(0, method$paa$w))
  expect_true(all(round(repr[[2]] - expected, 6) == 0))
})
