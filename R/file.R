#' @export
append_rda <- function(file_path, objects=character(), envir=parent.frame(), remove=FALSE, ...)
{
  # 'file' is the name of the file where the data will be saved; if 'file' doesn't exist it will be created.
  # 'objects' is a character vector containing the names of objects to be appended to 'file'.
  # 'envir' is the environment to search for objects to be saved.
  # If 'remove' is true, remove all other objects from 'file' besides those named in 'objects'.

  e <- new.env()
  .null <- NULL # 'save()' requires storage of at least one variable, so fake it.

  if (!file.exists(file_path))
    save(.null, file=file_path)

  load(file_path, envir=e)

  if (!remove) {
    for (o in objects)
      assign(o, get(o, envir=envir), envir=e)
    variables <- ls(e, all.names=TRUE)
  }
  else
    variables <- setdiff(ls(e, all.names=TRUE), objects)

  if (length(variables) == 0)
    variables <- ".null"

  save(list=variables, file=file_path, envir=e, ...)

  return (nop())
}


#' @export
clipwd <- function(use_dirname=TRUE, dir, source_files=TRUE, verbose=TRUE, ...)
{
  if (missing(dir))
    dir <- readClipboard() # Windows only, try 'scan("clipboard", what="")[1]' otherwise.

  if (use_dirname)
    dir <- dirname(dir)

  setwd(dir, ...)

  if (source_files) {
    files <- choose.files(filters=Filters[c("R"), ])
    sourceCommands <- NULL
    for (f in files) {
      #sourceCommand <- "source(\"./" %_% basename(f) %_% "\", keep.source=FALSE)"
      sourceCommand <- "source(\"" %_% normalizePath(f, '/') %_% "\", keep.source=FALSE)"
      sourceCommands <- c(sourceCommands, sourceCommand)
      if (verbose)
        cat("Running command '" %_% sourceCommand %_% "'.... ")
      ## N.B. 'writeClipboard()' automatically ends character strings with '\n'; convert to raw to prevent this.
      tryCatch(source(f, keep.source=FALSE), # Need to add extra "raw" to raw string to prevent deletion of last character.
        finally={ b <- charToRaw(paste(sourceCommands, collapse='\n')); b[length(b) + 1] <- as.raw(0); writeClipboard(b, format=1) })
      if (verbose) { cat("Done.", fill=TRUE); flush.console() }
    }
  }
}


#' @export
clip_wd <- function(..., source_files=FALSE)
{
  clipwd(..., source_files=source_files)
}
