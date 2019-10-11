is.config <- function(x) {
  if(!is.list(x))
    return(F)

  if (length(x) == 0) {
    return(T)
  } else {
    if(any(is.null(names(x))))
      return(F)

    if(any(names(x) == ""))
      return(F)
  }

  T
}

min_max <- function(x) {
  return(c(min(x), max(x)))
}


is.numeric_vector <- function(x) {
  if (!is.vector(x)) {
    return(FALSE)
  }

  if (any(!is.numeric(x)) | any(is.na(x)) | any(is.nan(x))) {
    return(FALSE)
  }

  return(TRUE)
}

is.character_vector <- function(x) {
  if (!is.vector(x)) {
    return(FALSE)
  }

  if (any(!is.character(x)) | any(is.na(x)) | ("" %in% x)) {
    return(FALSE)
  }
}

is.raw_numeric_vector <- function(x) {
  if (!is.numeric_vector(x)) {
    return(FALSE)
  }

  if (any(!is.null(names(x)))) {
    return(FALSE)
  }

  return(TRUE)
}

is.raw_character_vector <- function(x) {
  if (!is.character_vector(x)) {
    return(FALSE)
  }

  if (any(!is.null(names(x)))) {
    return(FALSE)
  }

  return(TRUE)
}

is.named_numeric_vector <- function(x) {
  if (!is.numeric_vector(x)) {
    return(FALSE)
  }

  if (is.null(names(x))) {
    return(FALSE)
  }

  if (!is.raw_character_vector(x)) {
    return(FALSE)
  }

  return(TRUE)
}

#' @export
write_float8 <- function(x, fp, size, append = T) {
  if (append)
    write_float8_c(x, fp, size, "a+b")
  else
    write_float8_c(x, fp, size, "w+b")
}

#' @export
write_uint2 <- function(x, fp, size, append = T) {
  if (append)
    write_uint2_c(x, fp, size, "a+b")
  else
    write_uint2_c(x, fp, size, "w+b")
}

#' @export
write_uint1 <- function(x, fp, size, append = T) {
  if (append)
    write_uint1_c(x, fp, size, "a+b")
  else
    write_uint1_c(x, fp, size, "w+b")
}

#' @export
read_float8 <- function(fp, size, pos1 = 0, pos2 = 1) {
  read_float8_c(fp, size, pos1, pos2)
}

#' @export
read_uint2 <- function(fp, size, pos1 = 0, pos2 = 1) {
  read_uint2_c(fp, size, pos1, pos2)
}

#' @export
read_uint1 <- function(fp, size, pos1 = 0, pos2 = 1) {
  read_uint1_c(fp, size, pos1, pos2)
}
