init.seas <- function(method) {
  method$TT <- 132
  method$L_1 <- 12
  method$w <- 12

  return(method)
}

#' @export represent.seas
represent.seas <- function(method, x) {
  m <- matrix(x, nrow = method$TT / method$L_1, ncol = method$L_1, byrow = T)
  seas <- apply(m, 2, mean)
  if (method$w  !=  method$L_1) {
    wl <- method$L_1 / method$w
    seas_aggregated <- rep(0, method$w)
    for(i in seq(method$L_1)) {
      seas_aggregated[floor((i - 1) / wl) + 1] <-
        seas_aggregated[floor((i - 1) / wl) + 1] + seas[i]
    }
    seas <- seas_aggregated / wl
  }

  return(seas)
}

distance.seas <- function(method, x, y) {
  sqrt(sum((x - y)^2)) * sqrt(method$TT / method$w)
}

#' @export
#' to_series.seas
to_series.seas <- function(method, x) {
  return(rep(x, each = method$L_1 / method$w, method$TT / method$L_1))
}
