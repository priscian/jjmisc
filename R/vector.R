#' @export
shift <- function(x, ...)
  UseMethod("shift")


#' @export
shift.default <- function (x, i=1L, roll=TRUE, na_rm=FALSE)
{
  if (i == 0L) return (x)

  naRm <- function(x, na_rm)
  {
    if (!na_rm) return (x)

    x[setdiff(seq_along(x), attr(na.omit(x), "na.action"))]
  }

  n  <- length(x)
  if (n == 0L) return (x)

  j <- i %% n

  if (roll && j == 0L) return (naRm(x, na_rm))

  if (!roll && j == 0L) {
    x[seq_along(x)] <- NA

    return (naRm(x, na_rm))
  }

  if (!roll && i > n) {
    rv <- x
    rv[seq_along(rv)] <- NaN
  }
  else {
    shifted <- 1L:(n - j)
    if (i > 0L)
      shifted <- (n - j + 1L):n


    if (!roll) x[shifted] <- NA
    if (na_rm) x[shifted] <- NaN

    rv <- x[c((n - j + 1L):n, shifted)]
    if (i > 0L)
      rv <- x[c(shifted, 1L:(n - j))]
  }

  if (na_rm)
    rv <- rv[!is.nan(rv)]

  return (rv)
}

## usage:
# shift(1:10)
# shift(1:10, roll=FALSE)
# shift(1:10, -1)
# shift(1:10, -1, roll=FALSE)
# shift(1:10, 5)
# shift(1:10, 5, roll=FALSE)
# shift(1:10, -5)
# shift(1:10, -5, roll=FALSE)
# shift(1:10, 5, roll=FALSE, na_rm=TRUE)
# shift(1:10, -5, roll=FALSE, na_rm=TRUE)


#' @export
shift.data.frame <- function(x, i, ...)
{
  if (!is.list(i)) {
    i <- as.list(rep(i, length.out=length(x)))
    names(i) <- names(x)
  }

  for(j in names(i))
    x[[j]] <- shift.default(x[[j]], i[[j]], ...)

  x
}


#' @export
chunk <- function(x, size, ...)
  UseMethod("chunk")


#'@export
chunk.default <- function(x, size, ...)
{
  split(x, as.numeric(gl(length(x), size, length(x))))
}


#'@export
chunk.data.frame <- function(x, size, ...)
{
  s <- chunk.default(seq(NROW(x)), size, ...)

  sapply(s, function(y) x[y, ], simplify = FALSE)
}


#'@export
chunk.matrix <- function(x, size, ...)
{
  chunk.data.frame(x, size, ...)
}


# http://stackoverflow.com/questions/16118050/how-to-check-if-a-vector-contains-n-consecutive-numbers
#' @export
seqle <- function(x, incr=1)
{
  if (!is.numeric(x)) x <- as.numeric(x)
  n <- length(x)
  y <- x[-1L] != x[-n] + incr
  #y <- abs(x[-1L] - x[-n] - incr) > .Machine$double.eps ^ 0.5 # Possible enhancement for numerics. See Web link above.
  i <- c(which(y | is.na(y)), n)

  list(lengths=diff(c(0L, i)), values=x[head(c(0L, i) +1L, -1L)])
}


## Find leading and trailing NAs in a vector; returns 'FALSE' for leading/trailing NAs, 'TRUE' for NA-enwrapped values.
#' @export
na_unwrap <- function(x, ...)
  UseMethod("na_unwrap")


#' @export
na_unwrap.matrix <- function(x, ...)
{
  apply(apply(x, 2, na_unwrap.default, ...), 1, any)
}


#' @export
na_unwrap.data.frame <- function(x, ...)
{
  na_unwrap.matrix(x, ...)
}


#' @export
na_unwrap.default <- function(x, type=c("both", "head", "tail", "none"), ...)
{
  type <- match.arg(type)

  nai <- stats:::na.omit.default(x) # Changed 14 Jan. 2017 to work with "ts" objects.
  #s <- rle(attr(nai, "na.action")) # See external function definition.
  s <- seqle(attr(nai, "na.action")) # See external function definition.

  leadi <- head(s$values, 1L)
  leadr <- NULL
  if (!is.na(leadi)) {
    if (leadi == 1L)
      leadr <- leadi:(leadi + head(s$lengths, 1L) - 1L)
  }

  traili <- tail(s$values, 1L)
  trailr <- NULL
  if (!is.na(traili)) {
    if (traili + tail(s$lengths, 1L) - 1L == length(x))
      trailr <- traili:(length(x))
  }

  r <- rep(TRUE, length(x))

  switch(type,
    both = r[c(leadr, trailr)] <- FALSE,
    head = r[c(leadr)] <- FALSE,
    tail = r[c(trailr)] <- FALSE
  )

  return (r)
}

## usage:
# na_unwrap(inst$Keeling)
# na_unwrap(inst$GISTEMP[inst$year %in% 1900:2000]) # No leading/trailing NAs.


## http://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
#' @export
split_at <- function(x, pos, split_after=FALSE, ...)
  UseMethod("split_at")


#' @export
split_at.data.frame <- function(x, pos, split_after=FALSE, simplify=FALSE, ...)
{
  sapply(split_at.default(seq(nrow(x)), pos=pos, split_after=split_after, ...),
    function(a)
    {
      x[a, ]
    }, simplify = simplify)
}


#' @export
split_at.default <- function(x, pos, split_after=FALSE, ...)
{
  if (is.logical(pos)) {
    if (length(pos) != length(x)) {
      warning("'pos' is not the same length as 'x', so it's being trimmed or repeated to match.")
      pos <- rep(pos, length.out=length(x))
    }
    pos <- which(pos)
  }

  unname(split(x, cumsum(seq_along(x) %in% (pos + as.integer(split_after)))))
}


## 'cumsum()' with 'na.rm=TRUE' equivalent.
#' @export
cum_sum <- function(x, ...) `[<-`(x, !is.na(x), cumsum(na.omit(x), ...))


## For non-decreasing dates, possibly with NAs, get 'diff()' whose sum equals last(x) - first(x).
#' @export
#' @importFrom zoo na.locf
diffs <- function(x, to_na=NULL, ...)
{
  r <- diff(zoo::na.locf(x, na.rm=FALSE), ...)
  if (!is.null(to_na))
    is.na(r) <- r %in% to_na

  r
}

## usage:
# x <- structure(c(NA, 16456, 16473, NA, NA, 16517, 16531, 16535, 16540, 16546, 16559, 16573, 16587, 16598, 16615, 16629, 16643, 16657, 16671, 16716, 16729, 16743, NA, 16772, 16783, 16805, 16820, 16834), class = "Date")
# diffs(x)


#' @export
nearest_below <- function(v, x, value=FALSE) { l <- which(v == max(v[(v < x)])); if (value) v[l] else l }

#' @export
nearest_above <- function(v, x, value=FALSE) { l <- which(v == min(v[(v > x)])); if (value) v[l] else l }


## Use convolution filter to calculate n-month moving average.
#' @export
moving_average <- function(x, n, sides=1L, ...) { if (is.null(n)) return (x); r <- stats::filter(x, rep(1/n, n), sides=sides, ...); colnames(r) <- colnames(x); return (r) } # 'n' is the window size.

#' @export
MA <- moving_average


#' @export
interpNA <- function (x, method=c("linear", "before", "after", "none"), unwrap=TRUE, skip_all_is_na=TRUE, ...)
{
  if (!inherits(x, "matrix") && !inherits(x, "timeSeries"))
    x <- as(x, "matrix")

  if (method[1] == "none")
    return (x)

  fun <- stats::approx
  if (method[1] %nin% c("linear", "before", "after", "none")) # '?stats::spline' for available "method"s.
    ## The following code removes any unmatched arguments from a call to 'FUN()';
    ## e.g. 'stats::spline()' doesn't have a formal argument 'f', which is nonetheless passed in below.
    fun <- function(...) { FUN <- stats::spline; d <- get_dots(...); a <- d$arguments[trimws(names(d$arguments)) %in% c("", formalArgs(FUN))]; do.call(FUN, a, quote=FALSE, envir=parent.frame()) }
  #else unwrap = FALSE

  interpVectorNA <- function(x, method, f, ...)
  {
    n <- length(x)
    idx <- (1:n)[!is.na(x)]
    y <- fun(x=idx, y=x[idx], xout=1:n, method=method, f=f)$y

    ## If spline interpolation, allow terminal NAs to be interpolated.
    if (!unwrap) return (y)

    ## If any leading/trailing NAs remain, interpolate them from the first/last value.
    y[!na_unwrap(y, "head")] <- y[head(which(!is.na(y)), 1)]
    y[!na_unwrap(y, "tail")] <- y[tail(which(!is.na(y)), 1)]

    r <- x
    r[na_unwrap(x, ...)] <- y[na_unwrap(x, ...)]

    r
  }

  method <- method[1]
  f <- 0
  if (method == "before") {
    method <- "constant"
    f <- 0
  }
  if (method == "after") {
    method <- "constant"
    f <- 1
  }
  for (i in 1:ncol(x)) {
    if (skip_all_is_na) {
      if (all(is.na(x[, i])))
        next
    }
    x[, i] <- interpVectorNA(x[, i], method, f, ...)
  }

  x
}


#' @export
backtick <- function(x, ...)
{
  sapply(x, function(a) paste("`", as.character(a), "`", sep=""), ...)
}
