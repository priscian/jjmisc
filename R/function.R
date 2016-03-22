#' Function Returning Nothing
#'
#' Return nothing via \code{\link[base]{invisible}()}.
#'
#' @param x R object to be returned invisibly.
#'
#' @seealso \code{\link[base]{invisible}}
#'
#' @examples
#' nop()
#'
#' @export
nop = function(x=NULL)
{
  return (invisible(x))
}


#' Resolve Triple-Dot (...) Argument inside Function Body
#'
#' Breaks up a triple-dot (\code{...}) argument of a function call into more accessible objects.
#'
#' @param ... A \code{...} argument composed of all non-formal arguments of a function call, where \code{...} must be a formal parameter of the function.
#'
#' @return A list of objects mostly related to the non-formal arguments of a function call:
#'   \item{calling_function}{The name (if available) of the calling function.}
#'   \item{current_function}{The name (if available) of the current function.}
#'   \item{current_formals}{A character vector of the names of the formal parameters of the current function.}
#'   \item{expanded}{A list of objects contained in \code{...}, with named elements for named arguments.}
#'   \item{as_character}{The objects contained in \code{...} coerced into a character vector, with named elements for named arguments.}
#'   \item{named_dots}{The names of the arguments contained in \code{...}, with null strings for unnamed arguments.}
#'   \item{named_args}{All named arguments of the function call, including formal arguments.}
#'
#' @examples
#' x = function(a, b, ...) browser()
#' x(a=666, fish="trout", frog=10, bob=3.14, T, "ouch", F)
#' dots = get_dots(...)
#' dots
#'
#' @export
get_dots = function(...)
{
  caller = sys.function(which=-1)
  if (!is.null(caller)) callerName = as.list(sys.call(-1))[[1]]
  formalArgs = names(formals(caller))
  expanded = substitute(...()) # List of '...' name-value pairs.
  dotsAsCharacter <- unlist(sapply(expanded, deparse, simplify=T))
  dotsNames = str_trim(names(dotsAsCharacter))
  if (is.null(dotsNames)) dotsNames = rep("", length(dotsAsCharacter))

  rv = list()
  if (!is.null(sys.call(-2))) rv$calling_function = as.list(sys.call(-2))[[1]]
  rv$current_function = callerName
  rv$current_formals = formalArgs
  rv$expanded = expanded
  rv$as_character = dotsAsCharacter
  rv$named_dots = dotsNames
  temp = append(formalArgs, dotsNames[dotsNames != ""], after=(which(formalArgs == "...")))
  rv$named_args = temp[temp != "..."]

  return (rv)
}


# Cordon off (or add to .GlobalEnv, globalenv(), etc.) objects in a function's internal environment.
cordon = function(fun, args=list(), envir=environment(), file=NULL, variables=NULL, copyArgs=F, action=c("run", "save", "load", "skip", "archive"))
{
  action = match.arg(action)
  .run = action == "run" || action == "save" || action == "load"
  .save = action == "save"
  .load = action == "load"
  .archive = action == "archive"

  if (.archive) {
    if (is.null(file)) file = paste(getwd(), "data", sep="/")
    if (!(file.info(file)$isdir)) file = dirname(file)
    Archive("load", file)
  }
  else if (.load)
    load(file=file, envir=envir)
  else if (.run) {
    tempEnv = new.env()

    # Add default arguments of 'fun' to argument list.
    formalArgs = formals(fun)
    for (f in names(formalArgs)) {
      if (!(f %in% names(args))) {
        if (!(is.symbol(formalArgs[[f]]) && formalArgs[[f]] == ""))
          args[[f]] = formalArgs[[f]]
      }
    }

    for (v in names(args))
      assign(v, args[[v]], envir=tempEnv)

    eval(body(fun), envir=tempEnv)

    if (is.null(variables))
      variables = setdiff(ls(tempEnv, all.names=T), names(args))
    if (copyArgs)
      variables = c(variables, names(args))

    if (!is.null(file)) {
      if (.save)
        save(list=variables, file=file, envir=tempEnv)
    }

    for (v in variables)
      assign(v, get(v, envir=tempEnv), envir=envir)
  }

  return (invisible(envir))
}
