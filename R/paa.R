init.paa <- function(method) {
  method$n <- 132
  method$w <- 6

  return(method)
}

set_config.paa <- function(method, config) {
  if ("n" %in% names(config)) {
    if ("w" %in% names(config)) {
      stopifnot(config$n %% config$w == 0)
      return(set_config.default(method, config))
    } else {
      stop("w missing")
    }
  } else {
    stop("n missing")
  }
}

#' @import TSrepr
represent.paa <- function(method, x) {
  stopifnot(length(x) == method$n)

  TSrepr::repr_paa(x, method$n / method$w, TSrepr::meanC)
}

distance.paa <- function(method, x, y) {
  stopifnot(length(x) == method$w)

  sqrt(sum((x - y)^2)) * sqrt(method$n / method$w)
}

res_symbols.paa <- function(method, x) {
  return(x)
}

loss.paa <- function(method, x, orig) {
  abs(orig - rep(x, each = method$n / method$w))
}
