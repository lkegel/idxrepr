init.ed <- function(method) {
  return(method)
}

represent.ed <- function(method, x) {
  return(x)
}

distance.ed <- function(method, x, y, tic = F) {
  if (tic) {
    return(d_ed_tic(x, y, length(x)))
  } else {
    return(sqrt(sum((x - y)^2)))
  }
}

to_series.ed <- function(method, x) {
  return(x)
}
