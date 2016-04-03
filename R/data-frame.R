#' @export
renumber  <- function(x, ...)
  UseMethod("renumber")


#' @export
renumber.pointer <- function(x, ...)
{
  if (inherits(..(x), "data.frame"))
    renumber.data.frame(x, ...)
  else
    stop("Pointer does not reference a relevant object type.")
}


#' @export
renumber.data.frame <- function(x, ...)
{
  row.names(..(x)) <- NULL
}


#' @export
vu_summary <- function(f, p, subset=TRUE, test=FALSE, digits=5L, overall=FALSE, verbose=TRUE, latex=FALSE, size="small", exclude1=FALSE, envir=parent.frame(), na_include=FALSE, continuous=5L, latex_corrections=list(jjmisc:::latex_correct_insert_bottom, jjmisc:::latex_correct_caption_position), summary...=list(), latex...=list(), print...=list(), ...)
{
  if (!inherits(p, "pointer"))
    stop("Function argument is not a pointer.")

  if (is.null(subset))
    subset <- TRUE

  dots <- get_dots(..., evaluate=TRUE)

  independent <- split(f)$term_labels
  for (i in independent) {
    if (exists(i, envir=envir)) {
      .v <- get(i, envir=envir)
    }
  }

  ## Use subset if dependent variable has missing observations.
  dependent <- split(f)$left_side
  if (!is.null(dependent)) {
    if (any(!complete.cases(..(p)[[dependent]])))
      subset <- complete.cases(..(p)[[dependent]]) & subset
  }

  summaryFormulaArgs = list(
    formula = f,
    data = ..(p),
    subset = subset,
    method="reverse",
    test = test,
    overall = overall,
    na.include = na_include,
    continuous = continuous
  )
  summaryFormulaArgs = modifyList(summaryFormulaArgs, summary...)

  .s <- try(do.call("summary", summaryFormulaArgs), silent=!verbose)

  if (inherits(.s, "try-error"))
    return (NULL)
  else {
    if (!latex) {
      printSummaryFormulaArgs = list(
        x = .s,
        long = TRUE,
        digits = digits,
        exclude1 = exclude1
      )
      ## Add '...' arguments to argument list.
      #printSummaryFormulaArgs <- c(modifyList(printSummaryFormulaArgs, dots$arguments[dots$named_dots != ""]), dots$arguments[dots$named_dots == ""])
      printSummaryFormulaArgs <- c(modifyList(printSummaryFormulaArgs, dots$evaluated[dots$named_dots != ""]), dots$evaluated[dots$named_dots == ""])
      printSummaryFormulaArgs = modifyList(printSummaryFormulaArgs, print...)

      do.call("print", printSummaryFormulaArgs)
    }
    else {
      latexArgs = list(
        object = .s,
        file = "",
        booktabs = TRUE,
        ctable = FALSE,
        here = FALSE,
        na.blank = TRUE,
        size = size,
        center = "center",
        long = TRUE,
        digits = digits,
        pctdig = 1L,
        exclude1 = exclude1,
        npct.size = "smaller[2]",
        Nsize = "smaller[1]",
        outer.size = "smaller[2]"
      )
      ## Add '...' arguments to argument list.
      #latexArgs <- c(modifyList(latexArgs, dots$arguments[dots$named_dots != ""]), dots$arguments[dots$named_dots == ""])
      latexArgs <- c(modifyList(latexArgs, dots$evaluated[dots$named_dots != ""]), dots$evaluated[dots$named_dots == ""])
      latexArgs <- modifyList(latexArgs, latex...)

      if (length(latex_corrections) > 0L) {
        l <- capture.output(do.call(Hmisc::latex, latexArgs))
        for (f in latex_corrections) {
          l <- do.call(f, list(l))
        }

        cat(l, sep='\n')
      }
      else {
        do.call(Hmisc::latex, latexArgs)
      }

      return (nop())
    }
  }
}


## Subset a data set and clean it up a bit all at once.
#' @export
subset_plus <- function(x, subpart, exclusions, inclusions, ...)
{
  p <- x
  if (!inherits(p, "pointer"))
    p <- ptr(x)

  if (!missing(subpart))
    x <- subset(..(p), subpart)
  else
    x <- subset(..(p))

  if (!missing(inclusions))
    x <- x[, inclusions]
  if (!missing(exclusions))
    x <- x[, setdiff(names(x), exclusions)]

  for (i in seq(ncol(x))) {
    if (inherits(x[, i], "mChoice")) {
      if (!missing(subpart))
        x[, i] <- update(x[, i], subpart)
    }
    else if (inherits(x[, i], "data.frame"))
      row.names(x[[names(x)[i]]]) <- NULL # Can't use x[, i] here, since it seems to pick out only one column.
  }

  drop_levels(ptr(x))
  renumber(ptr(x))

  return (x)
}


## Streamline use of 'subset_plus()' a little.
#' @export
spp <- function(x, y, subpart, ..., envir=globalenv(), summary_prefix="vs_", return_pointer=FALSE)
{
  p <- y
  if (!inherits(p, "pointer"))
    p <- ptr(y)

  d <- subset_plus(p, subpart=subpart, ...)
  vsd <- vu_summary

  varNames <- sapply(match.call(expand.dots=TRUE)[-1L], deparse)
  subVarName <- varNames["x"]
  envirVarName <- varNames["envir"]
  if (is.na(envirVarName))
    envirVarName <- deparse(formals()[["envir"]])

  assign(subVarName, d, envir=envir)
  assign(summary_prefix %_% subVarName, vsd, envir=envir)

  vsdp <- eval(substitute(ptr(vsdpName, envir=envir), list(vsdpName=summary_prefix %_% subVarName)))
  # N.B. Might need to make sure 'ptr()' is handling environments properly.
  formals(..(vsdp))$p <- eval_js("quote(ptr(\"" %_% subVarName %_% "\", envir=" %_% envirVarName %_% "))")

  if (return_pointer)
    return (ptr(subVarName), envir=envir)
  else
    return (nop())
}


#' @export
#' @importFrom gtools mixedsort
#' @importFrom pryr where
pgrep <- function(pattern="", ..., v="vs_", sort_names=FALSE, silent=FALSE, verbose=TRUE, value=TRUE, perl=TRUE, simplify=TRUE, envir=parent.frame())
{
  dots <- get_dots(...)

  namedArgs <- list()
  for (argName in dots$all_named_args) { # Get list of arguments to send into 'grep()'; no need to keep track of local args.
    if (!(argName %in% names(formals(grep)))) next
    val <- get(argName)
    if (is.null(val)) namedArgs[argName] <- list(NULL)
    else namedArgs[[argName]] <- val
  }

  parms <- dots$arguments
  parmNames <- dots$as_character

  rv <- list()

  for (i in seq_along(parms)) {
    p <- eval(parms[[i]], envir)

    obj <- NULL
    objName <- ""
    objEnv <- NULL

    if (is.numeric(p)) { # GOG legacy code; update for more generic application if necessary.
      editedProtcodes <- edited_protcodes(p)[[1L]]
      trimmedProtcode <- editedProtcodes$trimmed
      paddedProtcode <- editedProtcodes$padded

      obj <- get(v %_% trimmedProtcode)
      objName <- v %_% trimmedProtcode
    }
    else if (is.character(p)) {
      obj <- get(p)
      objName <- p
    }
    else if (is.pointer(p)) {
      obj <- ..(p)
      objName <- p$name
      objEnv <- environmentName(p$object)
    }
    else {
      obj <- p
      objName <- parmNames[i]
    }

    if (is.null(objEnv))
      objEnv <- environmentName(pryr::where(objName, envir))

    varNames <- names(obj)
    if (is.null(varNames))
      varNames <- character()
    if (sort_names) varNames <- gtools::mixedsort(varNames)

    namedArgs$x <- varNames

    out <- do.call("grep", namedArgs)
    rv[[objName]] <- out

    if (!silent) {
      if (verbose)
        catn("##### For object '" %_% objName %_% "' in <environment: " %_% objEnv %_% ">:")
      print(out)
    }
  }
  if (silent) {
    if (simplify && length(rv) == 1L)
      rv <- rv[[1L]]

    return (rv)
  }
}


## Quickly select columns from a data frame or matrix.
#' @export
select_df <- function(x, ..., subpart=NULL, show_always=NULL, view=FALSE, sort=c(NA, "index", "name"), allow_duplicates=FALSE, drop=FALSE, do_search=TRUE)
{
  # 'x' is a data frame or matrix.
  # '...' takes column names or column numbers of 'x' to be included in the output variable. If no column names or numbers are given, all the columns are kept. Unsyntactic column names should be enclosed in single or double quotes. Any expression evaluable in the calling context can be used; note that some expressions may need to be enclosed in backticks (`), and those enclosed in quotes will be processed as column names. Negative numbers will become offsets from the maximum column number, where -1 refers to ncol(x), -2 to ncol(x) - 1, -3 to ncol(x) - 2, etc. Ranges of column numbers such as 5:10 can be used; this is a consequence of the aforementioned expression evaluation.
  #   If 'p="<regex>"' is given as a named (non-formal) argument, its value will be used to call 'pgrep("<regex>", x, silent=T)' and put the result in situ among the other columns of '...'.
  # 'subpart' is a logical expression indicating elements or rows to keep, with missing values taken as false.
  # 'show.always' is a vector of column names that should always be included in the output variable whether they're explicitly named or not; should be set to null for no explicit inclusion.
  # 'view': logical; should results be displayed through the 'View()' function rather than returned?
  # 'sort' is the sort method for the selected column: NA for no sorting, "index" to order the columns as in 'x', or "name" to order the columns alphabetically.
  # 'allow.duplicates': should repeated column names/numbers be allowed? Note that the default behavior of the extraction operator '[.data.frame()' is to tack on sequential prefixes ".1", ".2", etc. to the name of subsequent instances of the column.
  # 'drop': logical, passed to '[.data.frame()'. If true the result is coerced to the lowest possible dimension. The default is to drop if only one column is left, but not to drop if only one row is left.
  # 'do.search': logical. If true, all the environments on the search path are checked for variables with the same names as the elements of '...'; matches are automatically 'deparse()'d without evaluation. If false, the time-consuming search is skipped, but the user must be careful to explicitly quote column names that have the same name as functions etc. in the search path, or risk errors.

  sort <- match.arg(sort)

  dots <- get_dots(...)

  cols <- seq_along(names(x))

  if (!is.null(dots$arguments)) {
    ## Which symbols can be evaluated as value types or in the calling context?
    evalFun <- function(a, b)
    {
      tryCatch({
        if (do_search) {
          if (length(find(deparse(a))) > 0L) # If 'deparse(a)' has the same name as any variable in the search path, skip evaluation.
            stop()
        }

        if (b == "p") {
          pgrep(a, x, silent=TRUE)
          #do.call(pgrep, list(pattern=a, x, silent=TRUE))
        }
        else
          eval(a)
      }, error=function(e) deparse(a))
    }

    cols <- mapply(evalFun, dots$arguments, dots$named_dots, SIMPLIFY=FALSE)

    if (!is.null(show_always))
      cols <- c(as.list(show_always), cols)

    ## Find the index of unevaluable symbols among the column names of 'x':
    cols <- unlist(sapply(cols, function(a) { r <- a; if (is.character(a)) r <- which(colnames(x) %in% a); r }))

    ## Eliminate any column numbers outside the range of '0:ncol(x)':
    cols <- cols[!(abs(cols) == 0L | abs(cols) > ncol(x))] # Not necessary to remove zeroes, though.

    ## Because negative and positive indices can't be mixed, turn negative values into offsets from the maximum column number:
    cols[cols < 0L] <- ncol(x) + cols[cols < 0L] + 1L

    if (!allow_duplicates)
      cols <- unique(cols)
  }

  if (!is.na(sort)) {
    if (sort == "index")
      cols <- sort(cols)
    else if (sort == "name")
      cols <- match(names(x)[cols][order(names(x)[cols])], names(x))
  }

  x <- x[, cols, drop=drop]

  if (!is.null(subpart))
    x <- subset_plus(x, subpart)

  if (view)
    View(x)
  else
    return (x)
}

#' @export
sdf <- select_df

## usage:
# x = structure(list(patid = c("002-0085-001", "002-0085-002", "002-0085-003", "002-0085-004", "002-0085-005"), protcode = structure(c(1L, 1L, 1L, 1L, 1L), .Label = c("0085", "0120", "0123", "0165", "0191", "0219"), class = c("labelled", "factor"), label = "Protocol number"), rx = c(1L, 2L, 1L, 2L, 1L), race = structure(c(1L, 2L, 2L, 2L, 2L), .Label = c("White", "Black", "Hispanic", "Asian", "other"), class = c("labelled", "factor"), label = "Race/Ethnicity"), age = structure(c(55.4388546657541, 28.1670088980151, 46.3545516769336, 54.1875427789185, 53.5905772302076), label = "Age", units = "years", class = "labelled")), .Names = c("patid", "protcode", "rx", "race", "age"), row.names = c(NA, 5L), class = "data.frame")
# sdf(x, patid, race, -1, 2:3)
# sdf(x, patid, "race", -1, 2:3) # Quotes required only for unsyntactic column names.
# sdf(x, patid, race, -1, 2:3, frog, `1 + 1 <- 2`) # Last two columns are unevaluable and not in column names of 'x', so are dropped.
# sdf(x, patid, race, -1, 2:3, 1) # Duplicate "patid" is dropped.
# sdf(x, patid, race, -1, 2:3, 1, allow_duplicates=TRUE)
# sdf(x, patid, race, -1, 2:3, subpart=(seq(nrow(x)) %in% 2:4))
# sdf(x, patid, race, -1, 2:3, subpart=(x$race == "Black"))
# sdf(x, patid, age, pgrep("^r", x, silent=TRUE), protcode)
# sdf(x, patid, age, p="^r", protcode) # Using special 'p' argument to call 'pgrep("^r", x, silent=TRUE)'; keeps argument ordering.
# sdf(x, patid, age, p="^r", bad="not.column", protcode, rx)
# sdf(x, patid, age, p="^r", bad="not.column", protcode, rx, allow_duplicates=TRUE)
# sdf(x, patid, age, p="^r", bad="not.column", protcode, rx, sort="index")
# sdf(x, patid, age, p="^r", bad="not.column", protcode, rx, sort="index", allow_duplicates=TRUE)
# sdf(x, patid, age, p="^r", bad="not.column", protcode, rx, sort="name")
# sdf(x, patid, age, p="^r", bad="not.column", protcode, rx, sort="name", allow_duplicates=TRUE)


## Quickly select columns from a data frame or matrix and display them using the 'View()' function.
#' @export
view_select_df <- function(x, ...)
{
  select_df(x, ..., view=TRUE)
}

#' @export
vdf <- view_select_df


## Row insertion into data frames etc.
#' @export
insert_row  <- function(x, newrow, rnum, ...)
  UseMethod("insert_row")


#' @export
insert_row.data.frame <- function(x, newrow, rnum) {
  x[seq(rnum + 1L, nrow(x) + 1L), ] <- x[seq(rnum, nrow(x)), ]
  x[rnum, ] <- newrow

  return (x)
}
