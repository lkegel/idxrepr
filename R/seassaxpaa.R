init.seassaxpaa <- function(method) {
  method$seas <- mgr_init("seas")
  method$seassax <- mgr_init("seassax")
  method$paa <- mgr_init("paa")

  return(method)
}

represent.seassaxpaa <- function(method, x) {
  seas <- mgr_represent(method$seas, x)
  seassax <- mgr_represent(method$seassax, x)

  res <- x - mgr_to_series(method$seas, seas)
  # res <- reorder.seaspaa(res, method$seas$L_1, method$paa$w, method$paa$n)
  respaa <- represent(method$paa, res)

  return(c(seassax, respaa))
}

min_max.seassaxpaa <- function(method, repr) {
  TT <- method$paa$n
  w <- method$paa$w
  L_1 <- method$seassax$seas$L_1

  qoff <- method$seassax$sax$qnorm
  x_off_1 <- qoff[repr[1:L_1] + 1]
  x_off_2 <- qoff[repr[1:L_1] + 2]

  x_paa <- repr[-seq(L_1)]

  x_min <- matrix(0, nrow = w, ncol = L_1)
  x_max <- matrix(0, nrow = w, ncol = L_1)
  for (idx_seas in seq(L_1)) {
    for (idx_res in (seq(w))) {
      x_min[idx_res, idx_seas] <- x_off_1[idx_seas] + x_paa[idx_res]
      x_max[idx_res, idx_seas] <- x_off_2[idx_seas] + x_paa[idx_res]
    }
  }

  return(list(x_min, x_max))
}

# distance.seaspaa <- function(method, x, y) {
#   L_1 <- method$seas$L_1
#   TT <- method$paa$n
#   w <- method$paa$w
#   f <- TT / (w * L_1)
#
#   d <- matrix(0, nrow = w, ncol = L_1)
#   for (idx_seas in seq(L_1)) {
#     for (idx_res in (seq(w))) {
#       d[idx_res, idx_seas] <- x[idx_seas] + x[idx_res + L_1] - y[idx_seas] - y[idx_res + L_1]
#     }
#   }
#
#   return(sqrt(sum(d^2 * f)))
# }


distance_R.seassaxpaa <- function(method, x, y) {
  TT <- method$paa$n
  w <- method$paa$w
  L_1 <- method$seassax$seas$L_1
  f <- TT / (w * L_1)
  xmm <- min_max.seassaxpaa(method, x)
  ymm <- min_max.seassaxpaa(method, y)

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

distance.seassaxpaa <- function(method, x, y) {
  w <- method$paa$w
  L_1 <- method$seassax$seas$L_1
  f <- method$paa$n / (w * L_1)

  return(d_seassaxpaa(x, y, w, L_1, f, method$seassax$sax$qnorm))
}

det_symbols.seassaxpaa <- function(method, x) {
  L_1 <- method$seassax$seas$L_1

  return(x[seq(L_1)])
}

res_symbols.seassaxpaa <- function(method, x) {
  L_1 <- method$seassax$seas$L_1

  return(x[-seq(L_1)])
}
