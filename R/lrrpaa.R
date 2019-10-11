init.lrrpaa <- function(method) {
  method$lrr <- mgr_init("lrr")
  method$paa <- mgr_init("paa")

  return(method)
}

represent.lrrpaa <- function(method, x) {
  lrr <- represent(method$lrr, x)

  res <- x - to_series(method$lrr, lrr)
  respaa <- represent(method$paa, res)

  repr <- unname(c(lrr, respaa))

  return(repr)
}

distance.lrrpaa <- function(method, x, y) {
  TT <- method$lrr$TT
  w <- method$paa$w

  x_off <- to_series(method$lrr, x[1])
  y_off <- to_series(method$lrr, y[1])
  d_off <- sum((x_off - y_off)^2)

  dpaa <- (distance.paa(method$paa, x[-1], y[-1]))^2

  return(sqrt(d_off + dpaa))
}

det_symbols.lrrpaa <- function(method, x) {
  return(x[1])
}

res_symbols.lrrpaa <- function(method, x) {
  return(x[-1])
}
