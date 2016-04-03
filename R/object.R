## Return the sorted names of an object.
#' @export
snames <- function(x, ...)
{
  return (sort(names(x), ...))
}


## Compare internal representations of R objects:
## http://stackoverflow.com/questions/9278715/value-reference-equality-for-same-named-function-in-package-namespace-environmen
#' @export
are_same <- function(x, y)
{
  f <- function(x) capture.output(.Internal(inspect(x)))
  all(f(x) == f(y))
}


#' @export
is_invalid <- function(x, ...)
{
  if (missing(x) || is.null(x) || length(x) == 0L)
    return (TRUE)

  if (is.list(x))
    return (all(sapply(x, is_invalid)))
  else if (is.vector(x))
    return (all(is.na(x)))
  else
    return (FALSE)
}
