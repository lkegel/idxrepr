init.sax <- function(method) {
  method$a <- 3

  method$dist_type <- "norm"
  method$dist_param <- list(mean = 0, sd = 1)
  method$qnorm <- qtable_calculate(method)
  method$lut <- lut_calculate(method)

  method$paa <- mgr_init("paa")

  return(method)
}

set_config.sax <- function(method, config) {

  if ("dist_type" %in% names(config)) {
    method$dist_type <- config$dist_type
  }

  if ("dist_param" %in% names(config)) {
    method$dist_param <- config$dist_param
  }

  if ("a" %in% names(config)) {
    method$a <- config$a
  }

  config$qnorm <- qtable_calculate(method)
  method$qnorm <- qtable_calculate(method)
  config$lut <- lut_calculate(method)

  set_config.default(method, config)
}

represent.sax <- function(method, x) {
  paa <- mgr_represent(method$paa, x)
  paa_to_sax.sax(method, paa)
}

distance_q.sax <- function(method, x, y) {
  sqrt(method$paa$n / method$paa$w * sum(cell_v(method, x, y)^2))
}

distance_lut.sax <- function(method, x, y, tic = F) {
  if (tic) {
    return(d_sax_tic(method$paa$n, method$paa$w, method$lut, x, y))
  } else {
    return(sqrt(method$paa$n / method$paa$w * d_sax(method$lut, x, y)))
  }
}

distance.sax <- distance_lut.sax

cell_v <- function(method, x, y) {
  # vectorize and replace sax_to_sax0
  if (all(abs(x - y) <= 1)) return(0)

  idx_1_gt_2 <- x > y

  temp <- x[!idx_1_gt_2]
  x[!idx_1_gt_2] <- y[!idx_1_gt_2]
  y[!idx_1_gt_2] <- temp

  idx_le_1 <- x - y <= 1

  inv <- method$qnorm[x + 1] - method$qnorm[y + 2]

  inv[idx_le_1] <- 0

  return(inv)
}

to_series.sax <- function(method, x) {
  lower <- rep(method$qnorm[x + 1], each = method$paa$n / method$paa$w)
  upper <- rep(method$qnorm[x + 2], each = method$paa$n / method$paa$w)
  return(rbind(lower, upper))
}

res_symbols.sax <- function(method, x) {
  return(x)
}

loss.sax <- function(method, x, orig) {
  method$qnorm[x + 2] - method$qnorm[x + 1]
}

#' @useDynLib idxrepr
#' @importFrom Rcpp sourceCpp
NULL

cell_offset_v <- function(method, x, y, xoff, yoff) {
  return(cell_offset_vc(method$qnorm, x, y, xoff, yoff))
}

cell_offset_sax_v <- function(method, x, y, sax, xoff, yoff) {
  return(cell_offset_sax_vc(method$qnorm, x, y, sax$qnorm, xoff, yoff))
}

#' @import distr
dist_calculate <- function(x, type, mod, param, qnorm = NA, a = NA) {
  if (type == "norm") {
    if(mod == "p") {
      return(pnorm(x, mean = param$mean, sd = param$sd))
    } else if (mod == "q") {
      return(qnorm(x, mean = param$mean, sd = param$sd))
    } else {
      stop("N/A")
    }
  } else if (type == "pearson_ds") {
    if (mod == "p") {
      ppearson <- PearsonDS::ppearson(x, moments = param)
      ppearson[ppearson == 0] <- 0.000001
      return(ppearson)
    } else if (mod == "q") {
      return(PearsonDS::qpearson(x, moments = param))
    }
  } else if (type == "user") {
    if (mod == "p") {
      result <- param$ecdf(x)
      result[result == 0] <- .Machine$double.eps
      return(result)
    } else if (mod == "q") {
      return(quantile(param$ecdf, x, names = F))
    } else {
      stop("N/A")
    }
  } else if (type == "outlier") {
    if (mod == "p") {
      x_ord <- order(x)
      repr <- rep(NA, length(x))
      i_lower <- 1
      for (i_ord in x_ord) {
        pivot <- x[i_ord]
        while (qnorm[i_lower] <= pivot) {
          i_lower <- i_lower + 1
        }
        # qnorm(i_lower) > pivot -> upper bound
        repr[i_ord] <- i_lower - 1
      }
      repr <- repr / a
      return(repr)
      # return(distr::p(param$dist)(x))
    } else {
      return(distr::q(param$dist)(x))
    }
  } else {
    stop("N/A")
  }
}

qtable_calculate <- function(method) {
  c(dist_calculate(seq(0, method$a) / method$a, method$dist_type, "q", method$dist_param))
}

lut_calculate <- function(method) {
  a <- method$a
  stopifnot(a^2 <= 2^20)
  lut <- matrix(0, a, a)
  for (i in seq(0, a - 1)) {
    for (j in seq(0, a - 1)) {
      lut[i + 1, j + 1] <- cell_v(method, i, j)
    }
  }

  return(lut)
}

paa_to_sax.sax <- function(method, x) {
  return(floor(dist_calculate(x, method$dist_type, "p", method$dist_param, method$qnorm, method$a) * method$a))
}

sax_to_sax0.sax <- function(method, x) {
  return(x)
}

sax0_to_paa.sax <- function(method, x) {
  return(method$qnorm[x + 1])
}
