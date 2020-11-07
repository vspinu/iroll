##' @importFrom Rcpp sourceCpp
##' @useDynLib iroll, .registration=TRUE
NULL

.reclass <- function(new, old, force_integer = FALSE){
  if(is(old, "Date"))
    as.Date(new, origin = .POSIXct(0))
  else if(inherits(old, "POSIXt"))
    as.POSIXct(new, tz = attr(old, "tz"))
  else if(is.integer(old) && !force_integer)
    new
  else
    as(new, class(old))
}

##' Rolling function for iregular series
##' @rdname iroll
##' @export
##' @param ix index converible to numeric (Date, POSIXct, etc). Must be ordered
##'     increasingly.
##' @param x value to be rolled over
##' @param left left bound scalar (convertible to numeric)
##' @param right right bound scalar (convertible to numeric)
##' @param left_open if the interval is left open
##' @param right_open if the interval is right open
##' @param fill number to fill for empty windows
iroll_mean <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA_real_) {
  out <- c_roll_mean(as.numeric(ix), x, left, right,
                     left_open, right_open, fill)
  .reclass(out, x)
}

##' @rdname iroll
##' @export
iroll_sd <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA_real_) {
  out <- c_roll_sd(as.numeric(ix), x,
                    as.numeric(left)[1], as.numeric(right)[1],
                    left_open, right_open, fill)
  .reclass(out, x)
}

##' @rdname iroll
##' @export
iroll_min <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA_real_) {
  out <- c_roll_min(as.numeric(ix), x,
                    as.numeric(left)[1], as.numeric(right)[1],
                    left_open, right_open, fill)
  .reclass(out, x, TRUE)
}

##' @rdname iroll
##' @export
iroll_max <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA_real_) {
  out <- c_roll_max(as.numeric(ix), x,
                    as.numeric(left)[1], as.numeric(right)[1],
                    left_open, right_open, fill)
  .reclass(out, x, TRUE)
}

##' @rdname iroll
##' @export
iroll_first <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA_real_) {
  out <- c_roll_first(as.numeric(ix), x,
                      as.numeric(left)[1], as.numeric(right)[1],
                      left_open, right_open, fill)
  .reclass(out, x, TRUE)
}

##' @rdname iroll
##' @export
iroll_last <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA_real_) {
  out <- c_roll_last(as.numeric(ix), x,
                     as.numeric(left)[1], as.numeric(right)[1],
                     left_open, right_open, fill)
  .reclass(out, x, TRUE)
}


##' @rdname iroll
##' @export
iroll_sum <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA_real_) {
  out <- c_roll_sum(as.numeric(ix), x,
                    as.numeric(left)[1], as.numeric(right)[1],
                    left_open, right_open, fill)
  .reclass(out, x, TRUE)
}

##' @rdname iroll
##' @export
iroll_prod <- function(ix, x, left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA_real_) {
  out <- c_roll_prod(as.numeric(ix), x,
                     as.numeric(left)[1], as.numeric(right)[1],
                     left_open, right_open, fill)
  .reclass(out, x, TRUE)
}

##' @rdname iroll
##' @export
##' @param prob numeric scalar in [0, 1].
iroll_quantile <- function(ix, x, left, right = 0,
                           left_open = FALSE, right_open = FALSE,
                           prob = .5, fill = NA_real_) {
  out <- c_roll_quantile(as.numeric(ix), x,
                         as.numeric(left)[1], as.numeric(right)[1],
                         left_open, right_open, fill)
  .reclass(out, x)
}
