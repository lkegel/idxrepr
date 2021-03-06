init <- function(method) UseMethod("init")
init.default <- function(method) stop("Unknown class")

get_config <- function(method) UseMethod("get_config")
get_config.default <- function(method) {
  unclass(method)
}

set_config <- function(method, config) UseMethod("set_config")
set_config.default <- function(method, config) {
  stopifnot(is.config(config))

  stopifnot(all(names(config) %in% names(method)))

  for (name in names(config)) {
    value <- config[[name]]
    if (is.null(value)) {
      stop()
    } else if (is.list(value)) {
      # do nothing
    } else if (any(is.na(value)) |  any(is.nan(value))) {
      stop()
    }
    method[[name]] <- config[[name]]
  }

  method
}

represent <- function(method, x) UseMethod("represent")
represent.default <- function(method, x) stop("Unknown class")

distance <- function(method, x, y, ...) UseMethod("distance")
distance.default <- function(method, x, y, ...) stop("Unknown class")

to_series <- function(method, x) UseMethod("to_series")
to_series.default <- function(method, x)
  stop("This class does not support to_series")

det_symbols <- function(method, x) UseMethod("det_symbols")
det_symbols.default <- function(method, x) stop("Unknown class")

res_symbols <- function(method, x) UseMethod("res_symbols")
res_symbols.default <- function(method, x) stop("Unknown class")

loss <- function(method, x, orig) UseMethod("loss")
loss.default <- function(method, x, orig) stop("Unknown class")
