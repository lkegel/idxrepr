init.seassax <- function(method) {
  method$seas <- idxrepr::mgr_init("seas")
  method$sax <- idxrepr::mgr_init("sax")

  return(method)
}

represent.seassax <- function(method, x) {
  paa <- unname(represent(method$seas, x))

  result <- paa_to_sax.sax(method$sax, paa)

  return(result)
}

distance.seassax <- function(method, x, y) {
  sqrt(method$seas$TT / method$seas$w) * sqrt(sum(cell_v(method$sax, x, y)^2))
}

det_symbols.seassax <- function(method, x) {
  L_1 <- method$seas$L_1

  return(x[seq(L_1)])
}

