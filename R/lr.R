init.lr <- function(method) {
  method$TT <- 132

  return(method)
}

set_config.lr <- function(method, config) {
  if ("TT" %in% names(config)) {
    return(set_config.default(method, config))
  } else {
    stop("TT missing")
  }
}

#' @export represent.lr
represent.lr <- function(method, x) {
  stopifnot(length(x) == method$TT)

  fit <- lm(as.numeric(x) ~ seq(0, length(x) - 1))
  theta_2 <- as.numeric(coefficients(fit)[2])
  theta_1 <- as.numeric(coefficients(fit)[1])

  return(c(theta_1, theta_2))
}

distance.lr <- function(method, x, y) {
  x_series <- to_series.lr(method, x)
  y_series <- to_series.lr(method, y)

  return(sqrt(sum((x_series - y_series)^2)))
}

#' @export to_series.lr
to_series.lr <- function(method, x) {
  return(x[2] * seq(0, method$TT -  1) + x[1])
}
