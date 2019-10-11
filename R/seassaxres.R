init.seassaxres <- function(method) {
  method$seas <- mgr_init("seas")
  method$seassax <- mgr_init("seassax")
  method$sax <- mgr_init("sax")

  method <- set_config(method)

  return(method)
}

set_config.seassaxres <- function(method, config) {
  method$sax$lut <- lut_calculate.seassaxres(method$sax)
  method$seassax$sax$lut <- lut_calculate.seassaxres(method$seassax$sax)

  return(method)
}

represent.seassaxres <- function(method, x) {
  seas <- mgr_represent(method$seas, x)
  seassax <- mgr_represent(method$seassax, x)


  res <- x - mgr_to_series(method$seas, seas)
  ressax <- mgr_represent(method$sax, res)

  return(c(seassax, ressax))
}

min_max.seassaxres <- function(method, repr) {
  TT <- method$sax$paa$n
  w <- method$sax$paa$w
  L_1 <- method$seassax$seas$L_1

  qoff <- method$seassax$sax$qnorm
  x_off_1 <- qoff[repr[1:L_1] + 1]
  x_off_2 <- qoff[repr[1:L_1] + 2]

  qres <- method$sax$qnorm
  idx_res <- seq(L_1 + 1, L_1 + w)
  x_res_1 <- qres[repr[idx_res] + 1]
  x_res_2 <- qres[repr[idx_res] + 2]


  x_min <- matrix(0, nrow = w, ncol = L_1)
  x_max <- matrix(0, nrow = w, ncol = L_1)
  for (idx_seas in seq(L_1)) {
    for (idx_res in (seq(w))) {
      x_min[idx_res, idx_seas] <- x_off_1[idx_seas] + x_res_1[idx_res]
      x_max[idx_res, idx_seas] <- x_off_2[idx_seas] + x_res_2[idx_res]
    }
  }

  return(list(x_min, x_max))
}

distance_r.seassaxres <- function(method, x, y) {
  TT <- method$sax$paa$n
  w <- method$sax$paa$w
  L_1 <- method$seassax$seas$L_1
  f <- TT / (w * L_1)

  xmm <- min_max.seassaxres(method, x)
  ymm <- min_max.seassaxres(method, y)

  d <- matrix(0, nrow = w, ncol = L_1)
  for (is in seq(L_1)) {
    for (ir in (seq(w))) {
      if (xmm[[1]][ir, is] > ymm[[2]][ir, is]) {
        d[ir, is] <- xmm[[1]][ir, is] - ymm[[2]][ir, is]
      } else if (ymm[[1]][ir, is] > xmm[[2]][ir, is]) {
        d[ir, is] <- ymm[[1]][ir, is] - xmm[[2]][ir, is]
      }
    }
  }

  return(sqrt(sum(d^2 * f)))
}

distance_q.seassaxres <- function(method, x, y) {
  w <- method$sax$paa$w
  L <- method$seassax$seas$L
  f <- method$sax$paa$n / (w * L)

  return(d_seassaxres(x, y, w, L, f, method$sax$qnorm, method$seassax$sax$qnorm))
}

distance.seassaxres <- function(method, x, y) {
  TT <- method$sax$paa$n
  B <- method$sax$paa$w
  L <- method$seassax$seas$L
  mres <- method$sax$lut
  mseas <- method$seassax$sax$lut
  stopifnot(sign(mres[1, 2]) != sign(mres[2, 1]))
  stopifnot(sign(mseas[1, 2]) != sign(mseas[2, 1]))

  return(d_seassaxres_lut(x, y, TT, B, L, mres, mseas))
}

lut_calculate.seassaxres <- function(method) {
  a <- method$a
  stopifnot(a^2 <= 2^20)
  lut <- matrix(0, a, a)
  for (i in seq(0, a - 1)) {
    for (j in seq(0, a - 1)) {
      lut[i + 1, j + 1] <- method$qnorm[i + 1] - method$qnorm[j + 2]
    }
  }

  return(lut)
}

det_symbols.seassaxres <- det_symbols.seassaxpaa

res_symbols.seassaxres <- res_symbols.seassaxpaa
