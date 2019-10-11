context("sax")

test_that("represent", {
  sax <- mgr_init("sax")
  x.raw <- rep(c(-0.44, -0.43, -0.1, 0.1, 0.43, 0.44),
               each = sax$paa$n / sax$paa$w)
  x.sax <- mgr_represent(sax, x.raw)
  expected <- c(0, rep(1, sax$paa$w - 2), 2)

  expect_equal(expected, x.sax)
})

test_that("represent2", {
  sax <- mgr_init("sax")

  sax <- mgr_set_config(sax, list(a = 4))
  x.raw <- rep(c(-0.68, -0.67,    0, 0.1, 0.67, 0.68),
               each = sax$paa$n / sax$paa$w)
  expected <-  c(  0,       1,    2,   2,    2,    3)
  x.sax <- mgr_represent(sax, x.raw)

  expect_equal(expected, x.sax)
})

test_that("represent3", {
  sax <- mgr_init("sax")
  paa <- mgr_init("paa")
  paa <- mgr_set_config(paa, list(n = 800, w = 8))
  sax <- mgr_set_config(sax, list(a = 4, paa = paa))
  x.raw <- rep(c(-0.68, -0.67, -0.66, -0.1,    0, 0.1, 0.67, 0.68), each = 100)
  expected <-  c(    0,     1,     1,    1,    2,   2,    2,    3)
  x.sax <- mgr_represent(sax, x.raw)

  expect_equal(expected, x.sax)
})

test_that("represent4", {
  sax <- mgr_init("sax")
  paa <- mgr_init("paa")
  paa <- mgr_set_config(paa, list(n = 800, w = 8))
  dp <- list(mean = 0, sd = 0.5)
  sax <- mgr_set_config(sax, list(a = 4, dist_param = dp, paa = paa))
  x.raw <- rep(c(-0.68, -0.67, -0.66, -0.1,    0, 0.1, 0.67, 0.68), each = 100)
  expected <-  c(    0,     0,     0,    1,    2,   2,    3,    3)
  x.sax <- mgr_represent(sax, x.raw)

  expect_equal(expected, x.sax)
})

test_that("cell and distance", {
  a <- 4

  sax <- mgr_init("sax")
  paa <- mgr_set_config(mgr_init("paa"), list(n = 800, w = 8))
  sax <- mgr_set_config(sax, list(a = a, paa = paa))

  gr <- data.frame(expand.grid(0:(a - 1), 0:(a - 1),
                               stringsAsFactors = F),
                   stringsAsFactors = F)
  gr <- cbind(gr, Result = NA_real_)
  gr[c(1, 2, 5, 6, 7, 10, 11, 12, 15, 16), 3] <- 0
  gr[c(3, 8, 9, 14), 3] <- qnorm(0.75)
  gr[c(4, 13), 3] <- 2 * qnorm(0.75)

  for (irow in seq(nrow(gr))) {
    expect_equal(gr[irow, 3], cell_v(sax, gr[irow, 1], gr[irow, 2]))
  }

  expect_equal(10 * sqrt(sum(gr[, 3]^2)), mgr_distance(sax, gr[, 1], gr[, 2]))
})

test_that("cell_offset_v", {
  a <- 4
  sax <- mgr_init("sax")
  paa <- mgr_set_config(mgr_init("paa"), list(n = 800, w = 8))
  sax <- mgr_set_config(sax, list(a = a, paa = paa))

  df <- data.frame(x = integer(), y = integer(), xoff = integer(), yoff = integer(), expected = numeric())
  # ----------------x  y  xoff  yoff  d
  df <- rbind(df, c(0, 0,    1,    0, 0))
  df <- rbind(df, c(0, 1,    1,    0, 0))
  df <- rbind(df, c(0, 2,    1,    0, 0))
  df <- rbind(df, c(0, 3,    1,    0, qnorm(.75) - (1 - qnorm(.75))))
  df <- rbind(df, c(1, 0,    1,    0, 1))
  df <- rbind(df, c(1, 1,    1,    0, 1 - qnorm(.75)))
  df <- rbind(df, c(1, 2,    1,    0, 0))
  df <- rbind(df, c(1, 3,    1,    0, 0))
  df <- rbind(df, c(2, 0,    1,    0, 1 + qnorm(.75)))
  df <- rbind(df, c(2, 1,    1,    0, 1))
  df <- rbind(df, c(2, 2,    1,    0, 1 - qnorm(.75)))
  df <- rbind(df, c(2, 3,    1,    0, 0))
  df <- rbind(df, c(3, 0,    1,    0, 1 + 2 * qnorm(0.75)))
  df <- rbind(df, c(3, 1,    1,    0, 1 + qnorm(0.75)))
  df <- rbind(df, c(3, 2,    1,    0, 1))
  df <- rbind(df, c(3, 3,    1,    0, 0))
  names(df) <- c("x", "y", "xoff", "yoff", "expected")

  df_y <- data.frame(x = df$y, y = df$x, xoff = df$yoff, yoff = df$xoff, expected = df$expected)
  df <- rbind(df, df_y)

  result <- cell_offset_v(sax, df$x, df$y, df$xoff, df$yoff)

  expect_true(all(result - df$expected < 1e6))
})

test_that("cell_offset_sax_vc", {
  qnorm <- c(-Inf, qnorm(seq(3) / 3))
  qoff  <- c(-Inf, seq(2), Inf)

  df <- data.frame(x = integer(), y = integer(), xoff = integer(), yoff = integer(), expected = numeric())
  # ----------------x  y  xoff  yoff  d
  df <- rbind(df, c(0, 1,    0,    2, 1))
  df <- rbind(df, c(0, 2,    0,    2, 2 + qnorm(2 / 3) - (1 + qnorm(1 / 3))))
  df <- rbind(df, c(0, 2,    0,    1, 2 * qnorm(2 / 3)))
  names(df) <- c("x", "y", "xoff", "yoff", "expected")

  result <- cell_offset_sax_vc(qnorm, df$x, df$y, qoff, df$xoff, df$yoff)
  expect_true(all(abs(result - df$expected) < 1e-6))
})


test_that("dist_calculate", {
  given <- dist_calculate(0, "norm", "p", list(mean = 1, sd = 1.5))
  expected <- pnorm(0, mean = 1, sd = 1.5)
  expect_equal(expected, given)

  given <- dist_calculate(0.5, "norm", "q", list(mean = -1, sd = 0.5))
  expected <- qnorm(0.5, mean = -1, sd = 0.5)
  expect_equal(expected, given)

  moments <- list(mean = 2, var = 2, skewness = 0.5, kurtosis = 2)
  given <- dist_calculate(0, "pearson_ds", "p", moments)
  expected <- PearsonDS::ppearson(0, moments = moments)
  expect_true(expected - given < 1e6)

  given <- dist_calculate(0.5, "pearson_ds", "q", moments)
  expected <- PearsonDS::qpearson(0.5, moments = moments)
  expect_equal(expected, given)
})

test_that("lut", {
  method <- mgr_init("sax")
  a <- 4
  method <- mgr_set_config(method, list(a = a))

  for (i in seq(0, a - 1)) {
    for (j in seq(0, a - 1)) {
      expect_equal(cell_v(method, i, j)^2, d_sax(method$lut, i, j))
    }
  }

  x <- sample(seq(0, a - 1), 4)
  y <- sample(seq(0, a - 1), 4)
  expect_equal(sum(cell_v(method, x, y)^2), d_sax(method$lut, x, y))

  expect_equal(distance_q.sax(method, x, y),
               d_sax_tic(method$paa$n, method$paa$w, method$lut, x, y)$distance)
})
