init.lrrespaa <- function(method) {
  method$lr <- idxrepr::mgr_init("lr")
  method$paa <- idxrepr::mgr_init("paa")

  return(method)
}

represent.lrrespaa <- function(method, x) {
  lr <- mgr_represent(method$lr, x)
  res <- x - mgr_to_series(method$lr, lr)
  res_paa <- mgr_represent(method$paa, res)

  return(c(lr, res_paa))
}


distance.lrrespaa <- function(method, x, y) {
  stop("N/A")
}


res_symbols.lrrespaa <- function(method, x) {
  return(x[c(-1, -2)])
}
