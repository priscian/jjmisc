#' Concatenate Strings Easily
#'
#' Allows quick chaining together of character strings.
#'
#' @param a R object to be converted to a character vector.
#' @param b R object to be converted to a character vector.
#' @param sep A character string to separate the terms; passed to \code{\link[base]{paste}()}.
#'
#' @return The concatenation of \code{a} and \code{b}.
#'
#' @seealso \code{\link[base]{paste}}
#'
#' @examples
#' who = "world"
#' "Hello " %_% who %_% "!"
#'
#' @export
`%_%` = function(a, b, sep='') paste(a, b, sep=sep)
