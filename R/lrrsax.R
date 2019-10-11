init.lrrsax <- function(method) {
  method$lrr <- idxrepr::mgr_init("lrr")
  method$a <- 3
  method$min <- -Inf
  method$max <- Inf

  # Derived attribute
  method$lim <- get_lim.lrrsax(method)
  method$qnorm <- seq(-method$lim, method$lim, length.out = method$a + 1)
  method$lut <- lut_calculate.lrrsax(method)

  return(method)
}

get_lim.lrrsax <- function(method) {
  atan(sqrt(1 / var(seq(method$lrr$TT)))) + .Machine$double.eps
}

set_config.lrrsax <- function(method, config) {
  method <- set_config.default(method, config)

  # Derived attribute
  method$lim <- get_lim.lrrsax(method)
  method$qnorm <- seq(-method$lim, method$lim, length.out = method$a + 1)
  method$lut <- lut_calculate.lrrsax(method)

  if (all(c("min", "max") %in% names(config))) {
    method$min <- config$min - .Machine$double.eps
    method$max <- config$max + .Machine$double.eps
    method$qnorm <- c(seq(-method$max, -method$min, length.out = method$a/2),
                      seq(method$min, method$max, length.out = method$a/2 + 1))
  }

  return(method)
}

represent.lrrsax <- function(method, x) {
  stopifnot(length(x) == method$lrr$TT)

  x <- represent(method$lrr, x)
  stopifnot(-method$lim <= x && x <= method$lim)

  return(max(which(method$qnorm <= x)) - 1)
}

distance_q.lrrsax <- function(method, x, y) {
  if (abs(x - y) <= 1) {
    return(0)
  } else {
    if (x < y) {
      # +2 for 0-base to 1-base index and upper interval value
      x_raw <- to_series(method$lrr, method$qnorm[x + 2])
      y_raw <- to_series(method$lrr, method$qnorm[y + 1])
    } else {
      # +2 for 0-base to 1-base index and upper interval value
      x_raw <- to_series(method$lrr, method$qnorm[x + 1])
      y_raw <- to_series(method$lrr, method$qnorm[y + 2])
    }
    return(distance.ed(NA, x_raw, y_raw))
  }
}

distance_lut.lrrsax <- function(method, x, y) {
  return(method$lut[x + 1, y + 1])
}

distance.lrrsax <- distance_q.lrrsax

lut_calculate.lrrsax <- function(method) {
  a <- method$a
  stopifnot(a^2 <= 2^20)
  lut <- matrix(0, a, a)
  for (i in seq(0, a - 1)) {
    for (j in seq(0, a - 1)) {
      lut[i + 1, j + 1] <- distance_q.lrrsax(method, i, j)
    }
  }

  return(lut)
}

det_symbols.lrrsax <- function(method, x) {
  return(x[1])
}
