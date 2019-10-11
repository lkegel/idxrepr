context("seaspaa")

test_that("unaggregated", {
  method <- mgr_init("seaspaa")
  expect_equal(2, length(method))

  method$seas$L_1 <- 4
  method$seas$w <- 4
  method$paa$w <- 33

  rnvl <- list(rep(seq(4), 33) + rnorm(132, sd = 0.01),
               rep(seq(4) + 4, 33) + rnorm(132, sd = 0.01))
  repr <- lapply(rnvl, represent, method = method)

  expected <- list(c(seq(4), rep(0, 33)),
                   c(seq(4) + 4, rep(0, 33)))

  expect_equal(round(repr[[1]]), expected[[1]])
  expect_equal(round(repr[[2]]), expected[[2]])

  di <- distance(method, repr[[1]], repr[[2]])
  expected <- sqrt(sum((rnvl[[1]] - rnvl[[2]])^2))
  expect_equal(round(di), round(expected))
})


# test_that("unaggregated", {
#   x_seas <- rep(seq(-2, 2), 10)
#   y_seas <- rep(seq(2, -2), 10)
#   x_res <- sample(c(-10, 0, 10), 50, replace = T)
#   y_res <- sample(c(-10, 0, 10), 50, replace = T)
#
#   x <- x_seas + x_res
#   y <- y_seas + y_res
#   rnvl <- list(x, y)
#
#   method <- mgr_init("seaspaa")
#   expect_equal(2, length(method))
#
#   method$seas$TT <- 50
#   method$seas$L_1 <- 5
#   method$seas$w <- 5
#   method$paa$w <- 10
#   method$paa$n <- 50
#
#   repr <- lapply(rnvl, represent, method = method)
#
#   distance(method, repr[[1]], repr[[2]]) / sqrt(sum((x - y)^2))
#
#
#   expected <- sqrt(sum((rnvl[[1]] - rnvl[[2]])^2))
#   expect_equal(round(di), round(sqrt(144 * 132)))
# })



