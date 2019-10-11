init.seaspaa <- function(method) {
  method$seas <- idxrepr::mgr_init("seas")
  method$paa <- idxrepr::mgr_init("paa")

  return(method)
}

represent.seaspaa <- function(method, x) {
  seas <- represent(method$seas, x)

  res <- x - to_series(method$seas, seas)
  # res <- reorder.seaspaa(res, method$seas$L_1, method$paa$w, method$paa$n)
  respaa <- represent(method$paa, res)

  repr <- unname(c(seas, respaa))

  return(repr)
}

reorder.seaspaa <- function(res, L_1, w, TT) {
  stopifnot(w %% L_1 == 0)
  idx <- as.vector(matrix(seq(TT), ncol = L_1, nrow = TT / L_1, byrow = T))

  return(res[idx])
}


# Distance for reorder / split
# distance.seaspaa <- function(method, x, y) {
#   L_1 <- method$seas$L_1
#   TT <- method$paa$n
#   w <- method$paa$w
#
#   idx_seas <- 1:L_1
#   idx_res <- (L_1 + 1):(2 * L_1)
#
#   s <- 0
#   for (i in seq(w / L_1)) {
#     idx_res <- (i * L_1 + 1):((i + 1) * L_1)
#     s <- s + sum((x[idx_seas] + x[idx_res] - (y[idx_seas] + y[idx_res]))^2)
#   }
#   return(sqrt(s) * sqrt(TT / w))
# }
distance_strong_constraint.seaspaa <- function(method, x, y) {
  L_1 <- method$seas$L_1
  TT <- method$paa$n
  w <- method$paa$w

  d <- rep(0, TT)
  for (tt in seq(TT)) {
    idx_seas <- (tt - 1) %% L_1 + 1
    idx_res <- floor((tt - 1) / L_1) + 1 + L_1
    d[tt] <- x[idx_seas] + x[idx_res] - y[idx_seas] - y[idx_res]
  }

  return(sqrt(sum(d^2)))
}

# softer_constraint
distance.seaspaa <- function(method, x, y) {
  L_1 <- method$seas$L_1
  TT <- method$paa$n
  w <- method$paa$w
  f <- TT / (w * L_1)

  d <- matrix(0, nrow = w, ncol = L_1)
  for (idx_seas in seq(L_1)) {
    for (idx_res in (seq(w))) {
      d[idx_res, idx_seas] <- x[idx_seas] + x[idx_res + L_1] - y[idx_seas] - y[idx_res + L_1]
    }
  }

  return(sqrt(sum(d^2 * f)))
}

det_symbols.seaspaa <- function(method, x) {
  L_1 <- method$seas$L_1

  return(x[seq(L_1)])
}

res_symbols.seaspaa <- function(method, x) {
  L_1 <- method$seas$L_1

  return(x[-seq(L_1)])
}

