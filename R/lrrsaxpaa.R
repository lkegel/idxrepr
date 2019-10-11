init.lrrsaxpaa <- function(method) {
  method$lrrsax <- mgr_init("lrrsax")
  method$paa <- mgr_init("paa")

  return(method)
}

represent.lrrsaxpaa <- function(method, x) {
  lrr <- represent(method$lrrsax$lrr, x)
  lrrsax <- represent(method$lrrsax, x)

  res <- x - to_series(method$lrrsax$lrr, lrr)
  respaa <- represent(method$paa, res)

  repr <- unname(c(lrrsax, respaa))

  return(repr)
}

min_max.lrrsaxpaa <- function(method, repr) {
  TT <- method$lrrsax$lrr$TT
  w <- method$paa$w

  qoff <- method$lrrsax$qnorm
  x_off_1 <- to_series(method$lrrsax$lrr, qoff[repr[1] + 1])
  x_off_2 <- to_series(method$lrrsax$lrr, qoff[repr[1] + 2])

  idx_paa <- rep(seq(2, w + 1), each = TT / w)
  x_paa <- repr[idx_paa]

  x_min <- rep(0, TT)
  x_max <- rep(0, TT)

  for(tt in seq(TT)) {
    x_min[tt] <- min(x_off_1[tt], x_off_2[tt]) + x_paa[tt]
    x_max[tt] <- max(x_off_1[tt], x_off_2[tt]) + x_paa[tt]
  }

  return(list(x_min, x_max))
}

debug.lrrsaxpaa <- function(method, x, x_min, x_max) {
  TT <- method$lrrsax$lrr$TT
  w  <- method$paa$w

  ma <- max(x_max)
  mi <- min(x_min)

  x_min_paa <- rep(0, w)
  x_max_paa <- rep(0, w)
  x_paa <- rep(0, w)

  for (i in seq(w)) {
    idx <- seq((i - 1) * (TT / w) + 1, i * (TT / w))
    x_min_paa[i] <- mean(x_min[idx])
    x_max_paa[i] <- mean(x_max[idx])
    x_paa[i] <- mean(x[idx])
  }

  plot(x_min_paa, ylim = c(mi, ma), t = "l")
  lines(x_max_paa, col = "blue")
  lines(x_paa, col = "red")
}

distance.lrrsaxpaa <- function(method, x, y,
                               debug = F, orig = NA) {
  TT <- method$lrrsax$lrr$TT
  x_mima <- min_max.lrrsaxpaa(method, x)
  y_mima <- min_max.lrrsaxpaa(method, y)

  if (debug) {
    debug.lrrsaxpaa(method, orig[[1]], x_mima[[1]], x_mima[[2]])
    debug.lrrsaxpaa(method, orig[[2]], y_mima[[1]], y_mima[[2]])
  }

  d <- rep(0, TT)
  for(tt in seq(TT)) {
    if        (x_mima[[1]][tt] > y_mima[[2]][tt]) {
      d[tt] <- x_mima[[1]][tt] - y_mima[[2]][tt]
    } else if (y_mima[[1]][tt] > x_mima[[2]][tt]) {
      d[tt] <- y_mima[[1]][tt] - x_mima[[2]][tt]
    }
  }

  return(sqrt(sum(d^2)))
}

det_symbols.lrrsaxpaa <- function(method, x) {
  return(x[1])
}

res_symbols.lrrsaxpaa <- function(method, x) {
  return(x[-1])
}
