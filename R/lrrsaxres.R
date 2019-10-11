init.lrrsaxres <- function(method) {
  method$lrrsax <- mgr_init("lrrsax")
  method$sax <- mgr_init("sax")

  return(method)
}

represent.lrrsaxres <- function(method, x) {
  lrr <- represent(method$lrrsax$lrr, x)
  lrrsax <- represent(method$lrrsax, x)

  res <- x - to_series(method$lrrsax$lrr, lrr)
  ressax <- represent(method$sax, res)

  repr <- unname(c(lrrsax, ressax))

  return(repr)
}

min_max.lrrsaxres <- function(method, repr) {
  TT <- method$lrrsax$lrr$TT
  w <- method$sax$paa$w

  qoff <- method$lrrsax$qnorm
  x_off_1 <- to_series(method$lrrsax$lrr, qoff[repr[1] + 1])
  x_off_2 <- to_series(method$lrrsax$lrr, qoff[repr[1] + 2])

  idx_sax <- rep(seq(2, w + 1), each = TT / w)
  x_sax <- repr[idx_sax]
  qnorm <- method$sax$qnorm
  x_sax_1 <- qnorm[x_sax + 1]
  x_sax_2 <- qnorm[x_sax + 2]

  x_min <- rep(0, TT)
  x_max <- rep(0, TT)

  for(tt in seq(TT)) {
    x_min[tt] <- min(x_off_1[tt], x_off_2[tt]) + x_sax_1[tt]
    x_max[tt] <- max(x_off_1[tt], x_off_2[tt]) + x_sax_2[tt]
  }

  return(list(x_min, x_max))
}

debug.lrrsaxres <- function(method, x, x_min, x_max) {
  TT <- method$lrrsax$lrr$TT
  w  <- method$sax$paa$w

  ma <- max(x_max)
  mi <- min(x_min)

  plot(x_min, ylim = c(mi, ma), t = "l")
  lines(x_max, col = "blue")
  lines(x, col = "red")

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


distance2.lrrsaxres <- function(method, x, y,
                               debug = F, orig = NA) {
  TT <- method$lrrsax$lrr$TT
  x_mima <- min_max.lrrsaxres(method, x)
  y_mima <- min_max.lrrsaxres(method, y)

  if (debug) {
    debug.lrrsaxres(method, orig[[1]], x_mima[[1]], x_mima[[2]])
    debug.lrrsaxres(method, orig[[2]], y_mima[[1]], y_mima[[2]])
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

distance_q.lrrsaxres <- function(method, x, y) {
  doff <- distance_q.lrrsax(method$lrrsax, x[1], y[1])^2
  dsax <- distance_q.sax(method$sax, x[-1], y[-1])^2

  return(sqrt(doff + dsax))
}

distance_lut.lrrsaxres <- function(method, x, y) {
  doff <- distance_lut.lrrsax(method$lrrsax, x[1], y[1])^2
  dsax <- distance_lut.sax(method$sax, x[-1], y[-1])^2

  return(sqrt(doff + dsax))
}

distance.lrrsaxres <- distance_lut.lrrsaxres

distance_seg.lrrsaxres <- function(method, x, y,
                               debug = F, x_orig = NA, y_orig = NA) {
  TT <- method$lrrsax$lrr$TT
  w <- method$sax$paa$w
  d <- rep(0, w)

  qoff <- method$lrrsax$qnorm
  x_off_1 <- to_series(method$lrrsax$lrr, qoff[x[1] + 1])
  x_off_2 <- to_series(method$lrrsax$lrr, qoff[x[1] + 2])

  y_off_1 <- to_series(method$lrrsax$lrr, qoff[y[1] + 1])
  y_off_2 <- to_series(method$lrrsax$lrr, qoff[y[1] + 2])

  qnorm <- method$sax$qnorm

  for (i in seq(w)) {
    idx <- seq((i - 1) * (TT / w) + 1, i * TT / w)
    x_min <- min(mean(x_off_1[idx]), mean(x_off_2[idx])) + qnorm[x[1 + i] + 1]
    x_max <- max(mean(x_off_1[idx]), mean(x_off_2[idx])) + qnorm[x[1 + i] + 2]

    y_min <- min(mean(y_off_1[idx]), mean(y_off_2[idx])) + qnorm[y[1 + i] + 1]
    y_max <- max(mean(y_off_1[idx]), mean(y_off_2[idx])) + qnorm[y[1 + i] + 2]

    if (x_min > y_max) {
      d[i] <- x_min - y_max
    } else if (y_min > x_max) {
      d[i] <- y_min - x_max
    }
  }

  return(sqrt(TT / w * sum(d^2)))
}

det_symbols.lrrsaxres <- det_symbols.lrrsaxpaa

res_symbols.lrrsaxres <- res_symbols.lrrsaxpaa
