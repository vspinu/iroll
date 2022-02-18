##' @importFrom Rcpp sourceCpp
##' @useDynLib iroll, .registration=TRUE
NULL

##' Rolling function for iregular series
##' @rdname iroll
##' @export
##' @param x value to be rolled over
##' @param ix index converible to numeric (Date, POSIXct, etc). Must be ordered
##'     increasingly.
##' @param left bound scalar (convertible to numeric)
##' @param right right bound scalar (convertible to numeric)
##' @param left_open if the interval is left open
##' @param right_open if the interval is right open
##' @param fill number to fill for empty windows
iroll_mean <- function(x, ix = seq_along(x), left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA) {
  c_roll_mean(x, as.numeric(ix), left, right,
              left_open, right_open, fill)
}

##' @rdname iroll
##' @export
iroll_sd <- function(x, ix = seq_along(x), left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA) {
  c_roll_sd(x, as.numeric(ix), left, right, left_open, right_open, fill)
}

##' @rdname iroll
##' @export
iroll_min <- function(x, ix = seq_along(x), left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA) {
  c_roll_min(x, as.numeric(ix), left, right, left_open, right_open, fill)
}

##' @rdname iroll
##' @export
iroll_max <- function(x, ix = seq_along(x), left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA) {
  out <- c_roll_max(x, as.numeric(ix), left, right, left_open, right_open, fill)
  out
}

##' @rdname iroll
##' @export
iroll_first <- function(x, ix = seq_along(x), left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA) {
  out <- c_roll_first(x, as.numeric(ix), left, right, left_open, right_open, fill)
  out
}

##' @rdname iroll
##' @export
iroll_last <- function(x, ix = seq_along(x), left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA) {
  c_roll_last(x, as.numeric(ix), left, right, left_open, right_open, fill)
}


##' @rdname iroll
##' @export
iroll_sum <- function(x, ix = seq_along(x), left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA) {
  c_roll_sum(x, as.numeric(ix), left, right, left_open, right_open, fill)
}

##' @rdname iroll
##' @export
iroll_prod <- function(x, ix = seq_along(x), left, right = 0, left_open = FALSE, right_open = FALSE, fill = NA) {
  c_roll_prod(x, as.numeric(ix), left, right, left_open, right_open, fill)
}

##' @rdname iroll
##' @export
##' @param prob numeric scalar in [0, 1].
iroll_quantile <- function(x, ix = seq_along(x), left, right = 0,
                           left_open = FALSE, right_open = FALSE,
                           prob = .5, fill = NA) {
  c_roll_quantile(x, as.numeric(ix), left, right, left_open, right_open, prob, fill)
}


## .reclass <- function(new, old, force_integer = FALSE){
##   if(is(old, "Date"))
##     as.Date(new, origin = .POSIXct(0))
##   else if(inherits(old, "POSIXt"))
##     as.POSIXct(new, tz = attr(old, "tz"))
##   else if(is.integer(old) && !force_integer)
##     new
##   else
##     as(new, class(old))
## }
