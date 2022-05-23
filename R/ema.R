
cumemaR <- function(x, ix = seq_along(x), n){
    N <- length(x)
    ex <- double(N)
    ex[[1]] <- x[[1]]
    for(i in seq_len(max(0, N-1L))){
        i1 <- i + 1L
        edelta <- exp(- (ix[[i1]] - ix[[i]])/n)
        ex[[i1]] <- ex[[i]]*edelta + x[[i1]]
    }
    ex
}

emaR <- function(x, ix = seq_along(x), n){
    N <- length(x)
    ex <- double(N)
    ex[[1]] <- x[[1]]
    for(i in seq_len(max(0, N-1L))){
        i1 <- i + 1L
        W <- exp(-(ix[[i1]] - ix[[i]])/n)
        ex[[i1]] <- W*ex[[i]] + (1-W)*x[[i1]]
    }
    ex
}

##' Exponential (weighted) moving averages and derivatives for iregular time
##' series.
##'
##' \code{fwd_xyz} versions are forward looking emas.
##' @param x values of the seires
##' @param ix time index
##' @param n ema parameter - number of periods (n = 1/theta where -theta is the
##'     exponent in the smoother). At \code{n} periods back \code{x} contributes
##'     about 36.8% of the total weight; at \code{3*n} it contributes about 5%
##'     to the average.
##' @param linear boolean, linear or not
##' @param cum boolean, cumulative or not
##' @export
ema <- function(x, ix = seq_along(x), n = 10, linear = F, cum = F) {
  n <- as.numeric(n)
  ix <- as.numeric(ix)
  f <-
    if(cum) c_cumema
    else if (linear) c_ema_lin
    else c_ema
  f(x, ix, n)
}

##' @export
##' @rdname ema
fwd_ema <- function(x, ix = seq_along(x), n = 10, linear = F, cum = F) {
    rev(ema(rev(x), -rev(as.numeric(ix)), n, linear, cum))
}

##' @rdname ema
##' @param weight weight
##' @export
wema <- function(x, weight, ix = seq_along(x), n = 10, linear = F, cum = T){
  n <- as.numeric(n)
  ix <- as.numeric(ix)
  num <- ema(x*weight, ix, n, linear, cum)
  den <- ema(weight, ix, n, linear, cum)
  out <- num/den
  ## avoid infinitity for num==0
  out[num == 0] <- 0
  out
}

##' @rdname ema
##' @param nslow nr of periods for slow moving EMA
##' @param nfast nr of periods for fast moving EMA
##' @export
macd <- function(x, ix = seq_along(x), nfast = 12, nslow = nfast*4, nbase = nfast*3, linear = F, cum = F){
  nfast <- as.numeric(nfast)
  nslow <- as.numeric(nslow)
  ix <- as.numeric(ix)
  fast <- ema(x, ix, nfast, linear, cum)
  slow <- ema(x, ix, nslow, linear, cum)
  out <- fast/slow - 1
  ## base <- ema(x, ix, nbase, linear, cum)
  ## out <- (fast - slow)/base
  out[is.nan(out)] <- 0
  out
}

##' @rdname ema
##' @export
wmacd <- function(x, weight, ix = seq_along(x), nfast = 12, nslow = nfast*4L, linear = F, cum = T){
  nfast <- as.numeric(nfast)
  nslow <- as.numeric(nslow)
  ix <- as.numeric(ix)
  fast <- wema(x, weight, ix, nfast, linear, cum)
  slow <- wema(x, weight, ix, nslow, linear, cum)
  out <- fast/slow - 1
  out[is.nan(out)] <- 0
  out
}

##' @rdname ema
##' @export
fwd_macd <- function(x, ix = seq_along(x), nfast = 12, nslow = nfast*4, linear = F, cum = F) {
  -rev(macd(rev(x), -rev(as.numeric(ix)), nfast, nslow, linear, cum))
}

##' @rdname ema
## emsd: Exponential moving standard deviation.
##' @export
emsd <- function(x, ix = seq_along(x), n = 10, normalize = T, linear = F, cum = F){
  n <- as.numeric(n)
  ix <- as.numeric(ix)
  emean <- ema(x, ix, n, linear, cum)
  dev <- (x - emean)^2L
  if (normalize)
    dev <- dev/(emean^2 + .Machine$double.eps)
  sqrt(ema(dev, ix, n, linear, cum))
}

##' @rdname ema
fwd_emsd <- function(x, ix = seq_along(x), n = 10, linear = F, cum = F) {
    rev(emsd(rev(x), -rev(as.numeric(ix)), n, linear, cum))
}

##' @rdname ema
## emsd: Exponential moving standard deviation.
##' @export
wemsd <- function(x, ix = seq_along(x), weight, n = 10, normalize = T, linear = F, cum = T){
  n <- as.numeric(n)
  ix <- as.numeric(ix)
  emean <- wema(x, ix, weight, n, linear, cum)
  dev <- (x - emean)^2L
  if (normalize)
    dev <- dev/(emean^2 + .Machine$double.eps)
  nsqrt(wema(dev, weight, ix, n, linear, F))
}

##' \code{xyz2} functions are two sided version (aka filters).
##' @rdname ema
##' @export
ema2 <- function(x, ix = seq_along(x), n = 10, linear = F, cum = F){
  n <- as.numeric(n)
  ix <- as.numeric(ix)
  f <-
    if(cum) c_cumema
    else if (linear) c_ema_lin
    else c_ema
  forw <- f(x, ix, n)
  back <- f(rev(x), -rev(as.numeric(ix)), n)
  (forw + rev(back))/2
}

##' @rdname ema
##' @export
wema2 <- function(x, ix = seq_along(x), weight, n = 10, linear = F, cum = F){
  n <- as.numeric(n)
  ix <- as.numeric(ix)
  num <- ema2(x*weight, ix, n, linear, cum)
  den <- ema2(weight, ix, n, linear, cum)
  out <- num/den
  ## avoid infinitity for num==0
  out[num == 0] <- 0
  out
}

##' @rdname ema
##' @export
macd2 <- function(x, ix = seq_along(x), nfast = 12, nslow = nfast*4, linear = F, cum = F){
  nfast <- as.numeric(nfast)
  nslow <- as.numeric(nslow)
  ix <- as.numeric(ix)
  fast <- ema2(x, ix, nfast, linear, cum)
  slow <- ema2(x, ix, nslow, linear, cum)
  out <- fast/slow - 1
  out[is.nan(out)] <- 0
  out
}
