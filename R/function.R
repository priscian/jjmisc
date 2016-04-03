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
nop <- function(x=NULL)
{
  return (invisible(x))
}


#' Resolve Triple-Dot (...) Argument inside Function Body
#'
#' Breaks up a triple-dot (\code{...}) argument of a function call into more accessible objects.
#'
#' @param ... A \code{...} argument composed of all non-formal arguments of a function call, where \code{...} must be a formal parameter of the function.
#' @param evaluate Logical: return \code{...} object not only as a list of symbols, but also a list of those symbols expanded by \code{eval()}.
#'
#' @return A list of objects mostly related to the non-formal arguments of a function call:
#'   \item{calling_function}{The name (if available) of the calling function.}
#'   \item{current_function}{The name (if available) of the current function.}
#'   \item{current_formals}{A character vector of the names of the formal parameters of the current function.}
#'   \item{arguments}{A list of objects, as symbols, contained in \code{...}, with named elements for named arguments.}
#'   \item{evaluated}{A list of objects contained in \code{...}, with named elements for named arguments.}
#'   \item{as_character}{The objects contained in \code{...} coerced into a character vector, with named elements for named arguments.}
#'   \item{named_dots}{The names of the arguments contained in \code{...}, with blank strings for unnamed arguments.}
#'   \item{all_named_args}{All named arguments of the function call, including formal arguments.}
#'
#' @examples
#' x <- function(a, b, ...) browser()
#' x(a=666, fish="trout", frog=10, bob=3.14, TRUE, "ouch", FALSE)
#' dots <- get_dots(...)
#' dots
#'
#' @export
get_dots <- function(..., evaluate=FALSE)
{
  caller <- sys.function(which=-1L)
  if (!is.null(caller))
    callerName <- as.list(sys.call(-1L))[[1L]]
  formalArgs <- names(formals(caller))
  #unevaluated <- substitute(...()) # List of '...' name-value pairs.
  unevaluated <- eval(substitute(alist(...)))
  dotsAsCharacter <- unlist(sapply(unevaluated, deparse, simplify=TRUE))
  dotsNames <- names(dotsAsCharacter)
  if (is.null(dotsNames))
    dotsNames <- rep("", length(dotsAsCharacter))

  rv <- list()
  if (!is.null(sys.call(-2L)))
    rv$calling_function <- as.list(sys.call(-2L))[[1L]]
  rv$current_function <- callerName
  rv$current_formals <- formalArgs
  #rv$... <- environment()$`...`
  rv$arguments <- as.list(unevaluated)
  if (evaluate)
    rv$evaluated <- list(...)
  rv$as_character <- dotsAsCharacter
  rv$named_dots <- dotsNames
  whichDots <- which(formalArgs == "...")
  if (length(whichDots) == 0L)
    whichDots <- ifelse(length(formalArgs) == 0L, 1L, length(formalArgs))
  temp <- append(formalArgs, dotsNames[dotsNames != ""], after=whichDots)
  rv$all_named_args <- temp[temp != "..."]

  return (rv)
}


make_current_timestamp <- function(fmt="%Y-%m-%d", use_seconds=FALSE, seconds_sep='+')
{
  sysTime <- Sys.time()
  timestamp <- format(sysTime, fmt)
  if (use_seconds)
    timestamp <- paste(timestamp, sprintf("%05d", lubridate::period_to_seconds(hms(format(Sys.time(), "%H:%M:%S")))), sep=seconds_sep)

  return (timestamp)
}

#' Evaluate Function inside an Environment and Extract and Save Any Useful Variables from Its Body
#'
#' Puts a function's body and its arguments into an environment for evaluation, and afterwards allows extraction of any variables from the body, not just a return value.
#'
#' @param fun The function to be evaluated.
#' @param ... Arguments to be passed into \code{fun}.
#' @param arguments A list of additional arguments for passing into \code{fun}; can be used e.g. when the formal arguments of \code{fun} conflict with those of the current function.
#' @param envir Environment where \code{variables} will be copied after \code{fun} has been evaluated. For \code{action <- "save"}, also names what variables in the evaluation environment will be \code{save()}d to an external file.
#' @param file_path For \code{action <- c("save", "load")}, the path to the file to which the \code{variables} in \code{envir} will be written, or from which objects will be extracted to \code{envir}. If \code{timestamp <- TRUE}, the file name provides a base name to which a timestamp is appended.
#' @param variables A character string naming variables among the arguments to, or in the body of, \code{fun} that will be extracted from the evaluation environment.
#' @param copy_args Logical: Should all named arguments to \code{fun} also be extracted from the evaluation environment (and for \code{action <- "save"}, saved)?
#' @param timestamp A logical value deciding whether a current timestamp (default format \code{%Y-%m-%d+[seconds after midnight]}) should be appended to the base file name given as part of \code{file_path}.
#' @param action A character string denoting the purpose of calling \code{cordon()} in the first place:
#' \tabular{ll}{
#'   run \tab Evaluate \code{fun} and extract variables, but don't load or save them. \cr
#'   save \tab Evaluate \code{fun}, extract variables, and save them to an external file. \cr
#'   load \tab Load saved data from \code{file_path}. If \code{timestamp <- TRUE}, load the most recent version according to the timestamped file name.\cr
#'   skip \tab Do nothing, i.e. prevent \code{fun} from being evaluated at all. \cr
#'   archive \tab Not implemented.
#' }
#'
#' @return The environment in which the body of \code{fun} was evaluated.
#'
#' @examples
#' \dontrun{
#' f <- function(x="frog", ...) { args <- get_dots(...)$arguments; nines <- args$beast + 333; bite <- args$bite; return (nop()) }
#' e <- cordon(f, bite="me", 3.14, beast=666, TRUE, envir=globalenv(), variables="nines")
#' get("nines", envir=globalenv())
#' e$bite
#' ls(e, all=TRUE)
#' }
#'
#' @export
cordon <- function(fun, ..., arguments=list(), envir=environment(), file_path=NULL, variables=NULL, copy_args=FALSE, timestamp=TRUE, timestamp...=list(), action=c("run", "save", "load", "skip", "archive"), verbose=TRUE)
{
  action <- match.arg(action)
  run_ <- action == "run" || action == "save" || action == "load"
  save_ <- action == "save"
  load_ <- action == "load"
  archive_ <- action == "archive"

  timestampArgs <- list(
    use_seconds = TRUE,
    seconds_sep = '+'
  )
  timestampArgs <- modifyList(timestampArgs, timestamp...)

  if (archive_) {
    if (is.null(file_path))
      stop("Archive file path must be specified.")
    if (!(file.info(file_path)$isdir)) file_path <- dirname(file_path)

    if (verbose) cat("Loading archive file \"" %_% filePath %_% "\".... ")
    archive("load", file_path) # 'archive()' not implemented yet.
    if (verbose) { cat("Done.", fill=TRUE); flush.console() }
  }
  else if (load_) {
    filePath <- file_path
    if (timestamp) {
      ## Get list of files in directory of 'file_path'.
      fileExt <- file_ext(file_path)
      dirName <- dirname(file_path)
      ## Find all versions of the file according to their timestamp extensions.
      filePaths <- sort(grep("^.*?_\\d{4}-\\d{2}-\\d{2}(?:\\" %_% timestampArgs$seconds_sep %_% "\\d{5})?\\." %_% fileExt %_% "$", list.files(dirName, full.names=TRUE), perl=TRUE, value=TRUE), decreasing=TRUE)
      if (length(filePaths) > 0L)
        ## Use the most recent version of the file according to its timestamp extension.
        filePath <- filePaths[1L]
    }

    if (verbose) cat("Loading data file \"" %_% filePath %_% "\".... ")
    load(file=filePath, envir=envir)
    if (verbose) { cat("Done.", fill=TRUE); flush.console() }
  }
  else if (run_) {
    evalEnv <- new.env()

    ## Add default arguments of 'fun' to argument list.
    argList <- as.list(formals(fun))
    hasDots <- FALSE
    if (!is.null(argList[["..."]])) hasDots <- TRUE
    argList[["..."]] <- NULL
    dots <- get_dots(...)
    ## Add '...' arguments to argument list.
    argList <- modifyList(argList, dots$arguments[dots$named_dots != ""]) # Replace duplicate named arguments with those from '...' and add new named arguments.
    argList <- c(argList, dots$arguments[dots$named_dots == ""]) # Tack on unnamed arguments from '...'.
    ## Add 'arguments' to 'argList'.
    argList <- modifyList(argList, arguments[names(arguments) != ""]) # Replace duplicate named arguments with those from 'arguments' and add new named arguments.
    argList <- c(argList, arguments[names(arguments) == ""]) # Tack on unnamed arguments from 'arguments'.

    temp <- fun
    body(temp) <- as.call(c(as.name("{"), expression(return (environment()))))
    ## Return environment containing complete set of new arguments, including '...', for 'fun()'.
    evalEnv <- do.call(temp, argList)

    ## Evaluate the body of 'fun()' in the environment created.
    eval(body(fun), envir=evalEnv)

    ## Pick out the variables to keep.
    if (is.null(variables))
      variables <- setdiff(ls(evalEnv, all=TRUE), c(names(formals(fun))))

    argEnv <- as.environment(argList[names(argList) != ""]) # Can only save named arguments.
    if (!is.null(file_path)) {
      if (save_) {
        filePath <- file_path
        if (timestamp)
          filePath <- paste(file_path_sans_ext(file_path), do.call("make_current_timestamp", timestampArgs), sep='_') %_% '.' %_% file_ext(file_path)

        if (verbose) cat("Saving data file \"" %_% filePath %_% "\".... ")
        save(list=variables, file=filePath, envir=evalEnv)
        if (copy_args)
          append_rda(filePath, objects=ls(argEnv, all=TRUE), envir=argEnv)
        if (verbose) { cat("Done.", fill=TRUE); flush.console() }
      }
    }

    for (v in variables)
      assign(v, get(v, envir=evalEnv), envir=envir)
    if (copy_args) {
      for (a in ls(argEnv, all=TRUE))
        assign(a, get(a, envir=argEnv), envir=envir)
    }

    return (invisible(evalEnv))
  }
}


#' @export
eval_js = function(..., envir=parent.frame(), enclos=if(is.list(envir) || is.pairlist(envir)) parent.frame() else baseenv())
{
  dots <- get_dots(..., evaluate=TRUE)
  expr <- unlist(dots$evaluated)

  if (is.list(expr)) {
    if (is.function(expr[[1L]])) # If first '...' argument is a function, execute it with other '...' arguments as its own.
      return (do.call(expr[[1L]], tail(expr, -1L)))

    for (i in expr) {
      if (is.expression(i) || is.language(i)) {
        return (eval(i, envir, enclos)) # Returns only the first expression found.
      }
    }
  }

  expr <- paste(expr, collapse=' ')

  if (typeof(expr) != "character")
    return (expr)

  expr <- parse(text=expr)
  eval(expr, envir, enclos)
}
