init.1d_sax <- function(method) {
  method$`1d_paa` <- idxrepr::mgr_init("1d_paa")
  method$sax_a <- idxrepr::mgr_init("sax")
  method$sax_s <- idxrepr::mgr_init("sax")

  return(method)
}

represent.1d_sax <- function(method, x) {
  repr <- represent(method$`1d_paa`, x)
  result <- rep(0, length(repr))

  idx_a <- seq(1, length(repr), by = 2)
  result[idx_a] <- paa_to_sax.sax(method$sax_a, repr[idx_a])

  idx_s <- seq(2, length(repr), by = 2)
  result[idx_s] <- paa_to_sax.sax(method$sax_s, repr[idx_s])

  # stopifnot(all(result > 0) || all(result[idx_a] < method$sax_a$a - 1) ||
  #             all(result[idx_s] < method$sax_s$a - 1))
  return(result)
}

distance.1d_sax <- function(method, x, y) {
  TT <- method$`1d_paa`$paa$n
  W <- method$`1d_paa`$paa$w

  d <- 0
  for (w in seq(W)) {
    seg_x <- x[(w - 1) * 2 + c(1, 2)]
    seg_y <- y[(w - 1) * 2 + c(1, 2)]
    d <- d + seg_distance_optim(method, seg_x, seg_y)
  }

  return(sqrt(d))
}

seg_distance <- function(method, x, y) {
  TT <- method$`1d_paa`$paa$n
  W <- method$`1d_paa`$paa$w

  a_x <- vapply(c(x[1], x[1] + 1), sax0_to_paa.sax, method = method$sax_a, FUN.VALUE = numeric(1))
  s_x <- vapply(c(x[2], x[2] + 1), sax0_to_paa.sax, method = method$sax_s, FUN.VALUE = numeric(1))
  a_y <- vapply(c(y[1], y[1] + 1), sax0_to_paa.sax, method = method$sax_a, FUN.VALUE = numeric(1))
  s_y <- vapply(c(y[2], y[2] + 1), sax0_to_paa.sax, method = method$sax_s, FUN.VALUE = numeric(1))

  lim <- c(min(a_x), min(s_x), min(a_y), min(s_y))
  lim <- cbind(lim, c(max(a_x), max(s_x), max(a_y), max(s_y)))
  gr <- apply(lim, 1, function(x) { c(x[1], x[2]) }) #(seq(x[1], x[2], length.out = 10) })

  fn <- function(x) {
    a <- x[1]
    s <- x[2]
    if (is.infinite(a) || is.infinite(s))
      return(Inf)
    b <- a - s * (TT / (W * 2))
    lr_x <- b + seq(0, TT / W - 1) * s

    a <- x[3]
    s <- x[4]
    if (is.infinite(a) || is.infinite(s))
      return(Inf)
    b <- a - s * (TT / (W * 2))
    lr_y <- b + seq(0, TT / W - 1) * s

    return(sum((lr_x - lr_y)^2))
  }

  min.value <- Inf
  min.param <- NA
  for (i_1 in seq(nrow(gr))) {
    for (i_2 in seq(nrow(gr))) {
      for (i_3 in seq(nrow(gr))) {
        for (i_4 in seq(nrow(gr))) {
          param <- c(gr[i_1, 1], gr[i_2, 2], gr[i_3, 3], gr[i_4, 4])
          value <- fn(param)
          if (value < min.value) {
            min.value <- value
            min.param <- param
          }
        }
      }
    }
  }

  return(min.value)
}


seg_distance_optim <- function(method, x, y) {
  TT <- method$`1d_paa`$paa$n
  W <- method$`1d_paa`$paa$w

  a_x <- vapply(c(x[1], x[1] + 1), sax0_to_paa.sax, method = method$sax_a, FUN.VALUE = numeric(1))
  s_x <- vapply(c(x[2], x[2] + 1), sax0_to_paa.sax, method = method$sax_s, FUN.VALUE = numeric(1))
  a_y <- vapply(c(y[1], y[1] + 1), sax0_to_paa.sax, method = method$sax_a, FUN.VALUE = numeric(1))
  s_y <- vapply(c(y[2], y[2] + 1), sax0_to_paa.sax, method = method$sax_s, FUN.VALUE = numeric(1))

  df <- data.frame(Ax = a_x, Sx = s_x, Ay = a_y, Sy = s_y)
  par <- apply(df, 2, mean)
  col_inf <- which(is.infinite(par))
  if (length(col_inf) > 0) {
    par[col_inf] <- apply(df, 2, function(x) return(x[which(is.finite(x))[1]]))[col_inf]
  }

  fr <- function(x) {
    a <- x[1]
    s <- x[2]
    if (is.infinite(a) || is.infinite(s))
      return(Inf)
    b <- a - s * (TT / (W * 2))
    lr_x <- b + seq(0, TT / W - 1) * s

    a <- x[3]
    s <- x[4]
    if (is.infinite(a) || is.infinite(s))
      return(Inf)
    b <- a - s * (TT / (W * 2))
    lr_y <- b + seq(0, TT / W - 1) * s

    return(sum((lr_x - lr_y)^2))
  }

  gr <- function(x) {
    Delta_a <- x[1] - x[3]
    Delta_s <- x[2] - x[4]
    WL <- TT / W
    da <- 2 * WL * Delta_a - WL * Delta_s
    da_prim <- - 2 * WL * Delta_a + WL * Delta_s
    ds <- Delta_a * (-WL) + 2 * Delta_s * (WL^3 / 4 - WL^3 / 2 + WL^2 / 2 + (WL * (WL - 1) * (2 * WL - 1)) / 6)
    ds_prim <- Delta_a * WL - 2 * Delta_s * (WL^3 / 4 - WL^3 / 2 + WL^2 / 2 + (WL * (WL - 1) * (2 * WL - 1)) / 6)

    return(c(da, ds, da_prim, ds_prim))
  }

  res <- optim(par, fr, gr, lower = df[1, ], upper = df[2, ], method = "L-BFGS-B")

  return(res$value)
}

det_symbols.1d_sax <- function(method, x) {
  return(x[seq(2, length(x), by = 2)])
}

res_symbols.1d_sax <- function(method, x) {
  return(x[seq(1, length(x), by = 2)])
}

