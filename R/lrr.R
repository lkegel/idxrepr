init.lrr <- function(method) {
  method$TT <- 132

  return(method)
}

set_config.lrr <- function(method, config) {
  if ("TT" %in% names(config)) {
    return(set_config.default(method, config))
  } else {
    stop("TT missing")
  }
}

represent.lrr <- function(method, x) {
  stopifnot(length(x) == method$TT)
  stopifnot(round(mean(x), 10) == 0)

  # x_t = theta_1 + seq(0, TT - 1) * theta_2
  fit <- lm(as.numeric(x) ~ seq(0, length(x) - 1))
  theta_1 <- as.numeric(coefficients(fit)[1])
  phi <- atan(-2 * theta_1 / (method$TT - 1))

  return(phi)
}

distance.lrr <- function(method, x, y) {
  x_series <- to_series(method, x)
  y_series <- to_series(method, y)

  return(sqrt(sum((x_series - y_series)^2)))
}

theta_1.lrr <- function(phi, TT) {
  -1 * tan(phi) * (TT - 1) / 2
}

theta_2.lrr <- function(theta_1, TT) {
  theta_2 <- -1 * theta_1 * 2 / (TT - 1)
}

to_series.lrr <- function(method, x) {
  theta_1 <- theta_1.lrr(x, method$TT)
  theta_2 <- theta_2.lrr(theta_1, method$TT)
  return(theta_2 * seq(0, method$TT -  1) + theta_1)
}

det_symbols.lrr <- function(method, x) {
  return(x[1])
}
