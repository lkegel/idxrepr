init.lrrressax <- function(method) {
  method$lrr <- mgr_init("lrr")
  method$sax <- mgr_init("sax")

  return(method)
}

represent.lrrressax <- function(method, x) {
  lrr <- represent(method$lrr, x)

  res <- x - to_series(method$lrr, lrr)
  ressax <- represent(method$sax, res)

  repr <- unname(c(lrr, ressax))

  return(repr)
}

distance.lrrressax <- function(method, x, y) {
  dlrr <- distance.lrr(method$lrr, x[1], y[1])^2
  dsax <- (distance.sax(method$sax, x[-1], y[-1]))^2

  return(sqrt(dlrr + dsax))
}

det_symbols.lrrressax <- det_symbols.lrr

res_symbols.lrrressax <- res_symbols.lrrpaa
