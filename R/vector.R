#' @export
shift <- function (x, i=1L, roll=TRUE, na_rm=FALSE)
{
  n  <- length(x)
  if (n == 0L) return(x)

  if (!roll && abs(i) >= n) {
    rv <- rep(NA, n)
    if (na_rm) rv <- vector(mode(x))

    return (rv)
  }

  j <- i %% n

  if (j == 0L) return(x)

  shifted <- 1L:(n - j)
  if (i > 0L)
    shifted <- (n - j + 1L):n

  if (!roll) x[shifted] <- NA
  if (na_rm) x[shifted] <- NaN

  rv <- x[c((n - j + 1L):n, shifted)]
  if (i > 0L)
    rv <- x[c(shifted, 1L:(n - j))]

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


#'@export
chunk <- function(x, size)
{
  split(x, as.numeric(gl(length(x), size, length(x))))
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
  apply(apply(x, 2, na_unwrap.default), 1, any)
}


#' @export
na_unwrap.data.frame <- function(x, ...)
{
  na_unwrap.matrix(x, ...)
}


#' @export
na_unwrap.default <- function(x, ...)
{
  nai <- na.omit(x)
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
  r[c(leadr, trailr)] <- FALSE

  return (r)
}

## usage:
# na_unwrap(inst$Keeling)
# na_unwrap(inst$GISTEMP[inst$year %in% 1900:2000]) # No leading/trailing NAs.


## http://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
#' @export
split_at <- function(x, pos, split_after=FALSE)
{
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
