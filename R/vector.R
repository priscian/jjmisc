#' @export
shift <- function (x, i=1L, roll=TRUE, na_rm=FALSE)
{
  n  <- length(x)
  if (n == 0L) return(x)

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
