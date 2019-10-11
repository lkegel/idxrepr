#' Initialize a representation.
#'
#' @param method_name A method name.
#' @return A representation method.
#' @export
mgr_init <- function(method_name) {
  method <- structure(list(), class = method_name)

  method <- init(method)

  method
}

#' Retrieve the configuration of a method.
#'
#' @param method A representation method.
#' @return A \code{list} of configuration parameters of \code{method}.
#' @export
mgr_get_config <- function(method) {
  get_config(method)
}

#' Set the configuration of a method.
#'
#' @param method A representation method.
#' @param config A \code{list} of configuration parameters for \code{method}.
#' @return The \code{method} with updated parameters.
#' @export
mgr_set_config <- function(method, config) {
  if (!is.list(config)) {
    stop("Configuration have to be a list.")
  }

  set_config(method, config)
}

#' @export
mgr_represent <- function(method, x) {
  if (exists("dbg", envir = globalenv())) stopifnot(is.raw_numeric_vector(x))

  result <- represent(method, x)

  if (exists("dbg", envir = globalenv())) stopifnot(is.numeric_vector(result))

  result
}

#' @export
mgr_distance <- function(method, x, y, ...) {
  if (exists("dbg", envir = globalenv())) {
    stopifnot(is.numeric_vector(x))
    stopifnot(is.numeric_vector(y))
  }

  stopifnot(length(x) == length(y))

  result <- distance(method, x, y, ...)

  if (exists("dbg", envir = globalenv()))
    stopifnot(is.raw_numeric_vector(result))

  return(result)
}

#' @export
mgr_to_series <- function(method, x) {
  return(to_series(method, x))
}

#' @export
mgr_det_symbols <- function(method, x) {
  return(det_symbols(method, x))
}


#' @export
mgr_res_symbols <- function(method, x) {
  return(res_symbols(method, x))
}


#' @export
mgr_loss <- function(method, x, orig) {
  return(loss(method, x, orig))
}


