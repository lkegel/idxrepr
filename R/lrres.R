init.lrres <- function(method) {
  method$lr <- idxrepr::mgr_init("lr")
  method$sax <- idxrepr::mgr_init("sax")

  return(method)
}

represent.lrres <- function(method, x) {
  lr <- mgr_represent(method$lr, x)
  res <- x - mgr_to_series(method$lr, lr)
  res_sax <- mgr_represent(method$sax, res)

  return(c(lr, res_sax))
}


distance.lrres <- function(method, x, y) {
  idx_sax <- rep(seq(3, length(x)),
                 each = method$sax$paa$n / method$sax$paa$w)

  d <- cell_offset_v(method$sax,
                     x[idx_sax],
                     y[idx_sax],
                     to_series(method$lr, x[1:2]),
                     to_series(method$lr, y[1:2]))

  return(sqrt(sum(d^2)))
}


res_symbols.lrres <- function(method, x) {
  return(x[c(-1, -2)])
}

loss.lrres <- function(method, x, orig) {
  trend <- to_series(method$lr, x[1:2])
  lower <- rep(method$sax$qnorm[x[c(-1, -2)] + 1],
               each = method$sax$paa$n / method$sax$paa$w)
  upper <- rep(method$sax$qnorm[x[c(-1, -2)] + 2],
               each = method$sax$paa$n / method$sax$paa$w)
  lower <- lower + trend
  upper <- upper + trend

  di_lower <- abs(orig - lower)
  di_upper <- abs(upper - orig)
  di <- di_lower
  idx <- di_upper > di_lower
  di[idx] <- di_upper[idx]

  return(sqrt(sum(di^2)))
}
