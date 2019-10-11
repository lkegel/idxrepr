context("seas")

test_that("unaggregated", {
  method <- mgr_init("seas")
  expect_equal(3, length(method))

  rnvl <- list(rep(seq(12), 11), rep(seq(12) + 12, 11))
  repr <- lapply(rnvl, represent.seas, method = method)

  expected <- seq(12)

  expect_equal(expected, repr[[1]])
  expect_equal(expected + 12, repr[[2]])

  di <- distance.seas(method, repr[[1]], repr[[2]])
  expect_equal(sqrt(144 * 132), di)
})

test_that("aggregated", {
  method <- mgr_init("seas")
  method$w <- 3

  rnvl <- list(rep(seq(12), 11), rep(seq(12) + 12, 11))
  repr <- lapply(rnvl, represent.seas, method = method)

  expected <- c(sum(1:4), sum(5:8), sum(9:12)) / 4

  expect_equal(expected, repr[[1]])
  expect_equal(expected + 12, repr[[2]])

  di <- distance.seas(method, repr[[1]], repr[[2]])
  expect_equal(sqrt(144 * 132), di)
})
