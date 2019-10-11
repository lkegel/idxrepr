init.basesax <- function(method) {
  method$base <- idxrepr::mgr_init("base")

  method$sax <- idxrepr::mgr_init("sax")
  method$sax$paa <- mgr_set_config(idxrepr::mgr_init("paa"), list(n = 1, w = 1))

  return(method)
}

represent.basesax <- function(method, x) {
  base <- unname(represent(method$base, x))

  result <- paa_to_sax.sax(method$sax, base)

  return(result)
}

distance.basesax <- function(method, x, y) {
  return(sqrt(method$base$TT * cell_v(method$sax, x, y)^2))
}
