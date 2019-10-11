init.base <- function(method) {
  method$TT <- 100

  return(method)
}

set_config.base <- function(method, config) {
  if ("TT" %in% names(config)) {
    return(set_config.default(method, config))
  } else {
    stop("TT missing")
  }
}

represent.base <- function(method, x) {
  stopifnot(length(x) == method$TT)

  return(mean(x))
}

distance.base <- function(method, x, y) {
  x_series <- to_series.base(method, x)
  y_series <- to_series.base(method, y)

  return(sqrt(sum((x_series - y_series)^2)))
}

to_series.base <- function(method, x) {
  return(rep(x[1], method$TT))
}
