init.seasres <- function(method) {
  method$seas <- mgr_init("seas")
  method$sax <- mgr_init("sax")

  return(method)
}

represent.seasres <- function(method, x) {
  seas <- mgr_represent(method$seas, x)
  res <- x - mgr_to_series(method$seas, seas)
  res_sax <- mgr_represent(method$sax, res)

  return(c(seas, res_sax))
}

distance.seasres <- function(method, x, y) {
  if (method$seas$L_1 / method$seas$w ==
      method$sax$paa$n / method$sax$paa$w) {

    idx_seas <- (seq(method$sax$paa$w) - 1) %% method$seas$w + 1

    d <- cell_offset_v(method$sax,
                       x[seq(-1, -method$seas$w)],
                       y[seq(-1, -method$seas$w)],
                       x[idx_seas],
                       y[idx_seas])

    return(sqrt(sum(d^2)) * sqrt(method$seas$L_1 / method$seas$w))
  } else {
    stop("N/A")
  }

}
