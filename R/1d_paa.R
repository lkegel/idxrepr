init.1d_paa <- function(method) {
  method$lr <- mgr_init("lr")
  method$paa <- mgr_init("paa")
  method$lr$TT <- method$paa$n / method$paa$w

  return(method)
}

represent.1d_paa <- function(method, x) {
  TT <- method$paa$n
  W <- method$paa$w

  repr <- c()
  for (w in seq(W)) {
    segment <- x[((w - 1) * (TT / W) + 1):(w * (TT / W))]
    segment.repr <- represent(method$lr, segment)
    s <- segment.repr[2]
    b <- segment.repr[1]
    a <- s * (TT / (W * 2)) + b
    segment.repr[1] <- a
    repr <- c(repr, segment.repr)
  }

  return(repr)
}

distance.1d_paa <- function(method, x, y) {
  x_series <- to_series(method, x)
  y_series <- to_series(method, y)

  return(sqrt(sum((x_series - y_series)^2)))
}

to_series.1d_paa <- function(method, x) {
  TT <- method$paa$n
  W <- method$paa$w

  series <- c()
  for (w in seq(W)) {
    segment.repr <- x[(2 * w - 1) + c(0, 1)]
    s <- segment.repr[2]
    a <- segment.repr[1]
    b <- a - s * (TT / (W * 2))
    segment <- to_series(method$lr, c(b, s))
    series <- c(series, segment)
  }

  return(series)
}

