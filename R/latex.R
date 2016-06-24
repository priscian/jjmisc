#' @export
make_sweave_document <- function(file_path, output_path="./", graphics_path="../images/", header, document="", bib, template_path)
{
  fileNameBase <- "output"
  if (missing(file_path))
    filepath_ <- "./" %_% fileNameBase %_% ".snw"
  else
    filepath_ <- file_path

  if (missing(template_path))
    templatePath <- system.file("templates", "report-template.snw", package="jjmisc")
  else
    templatePath <- template_path

  hastitle_ <- "false"
  if (!missing(header)) {
    hastitle_ <- "true"

    if (!is.null(header$title)) title_ <- header$title
    else title_ <- ""

    if (!is.null(header$author)) author_ <- header$author
    else author_ <- ""

    if (!is.null(header$date)) date_ <- header$date
    else date_ <- "\\\\today"
  }

  ## Separate document parts with vertical whitespace.
  document_ <- document

  hasbib_ <- "false"
  bibstylepath_ <- "C:/common/tex/jjava"
  bibpath_ <- "C:/common/tex/gog"

  if (!missing(bib)) {
    hasbib_ <- "true"

    if (!is.logical(bib)) {
      if (!is.null(bib$bibstylepath)) bibstylepath_ <- bib$bibstylepath
      if (!is.null(bib$bibpath)) bibpath_ <- bib$bibpath
    }
    else {
      if (!bib) hasbib_ <- "false"
    }
  }

  ## Load template.
  s <- readLines(templatePath)

  ## Replace appropriate placeholders.
  s <- gsub("@@HASTITLE@@", hastitle_, s)
  if (hastitle_ == "true") {
    s <- gsub("@@TITLE@@", title_, s)
    s <- gsub("@@AUTHOR@@", author_, s)
    s <- gsub("@@DATE@@", date_, s)
  }

  w <- head(which(grepl("@@DOCUMENT@@", s)), 1L)
  s <- append(s, document_, w)[-w]

  s <- gsub("@@HASBIB@@", hasbib_, s)
  if (hasbib_ == "true") {
    s <- gsub("@@BIBSTYLEPATH@@", bibstylepath_, s)
    s <- gsub("@@BIBPATH@@", bibpath_, s)
  }

  s <- gsub("@@FILENAME@@", filepath_, s)
  s <- gsub("@@OUTPUT@@", output_path %_% basename(file_path_sans_ext(filepath_)) %_% ".tex", s)

  s <- gsub("@@GRAPHICSPATH@@", graphics_path, s)

  ## Write output document.
  writeLines(s, filepath_)

  return (nop())
}


#' @export
make_latex_chunk <- function(contents="", type=c("code", "source", "graphics"), caption, label)
{
  type <- match.arg(type)

  if (missing(caption)) caption <- ""
  captionLatex <- "\\caption{" %_% caption
  captionLatex <- captionLatex %_% ifelse(!missing(label), "\\label{fig:" %_% label %_% "}}", "}")

  ## Separate chunk contents with vertical whitespace.
  contents <- c(contents, character(length(contents)))
  contents <- head(as.vector(matrix(contents, nrow=2L, byrow=TRUE)), -1L)

  chunk <- switch(type,
    "source" = c(
      "<<echo=F, results=hide>>=",
      "  #tryCatch(source(\"" %_% contents[1L] %_% "\"), error=function(e) e)",
      "  source(\"" %_% contents[1L] %_% "\")",
      "@"),
    "graphics" = c(
      "\\begin{figure}[!ht]",
      "\\begin{center}",
      "<<echo=F, fig=T>>=",
      paste("  ", contents, sep=''),
      "@",
      "  " %_% captionLatex,
      "\\end{center}",
      "\\end{figure}"),
    c( # Default option
      "<<echo=F, results=tex>>=",
      paste("  ", contents, sep=''),
      "@")
  )
  chunk <- c(chunk, "")

  return (chunk)
}


#' @export
build_report_document <- function(file_path_base, study_title, study_code, investigator, output_path="./report/", make_graphics_chunk=FALSE)
{
  if (missing(file_path_base)) file_path_base <- "studyName"
  if (missing(study_title)) study_title <- "Title of Study"
  if (missing(study_code)) study_code <- NULL
  if (missing(investigator)) investigator <- ""

  document <- c(
    make_latex_chunk(file_path_base %_% ".R", type="source"),
    "%%%%% Tables", "",
    make_latex_chunk("show_patient_characteristics()")
  )
  if (make_graphics_chunk)
    document <- c(document, make_latex_chunk("OutputGraphics()", type="graphics", caption="Some graphical output.", label="graphics_out"))

  header <- list(
    title = study_title %_% "\\\\\\\\" %_% ifelse(is.null(study_code), "", "{\\\\smaller[2](" %_% study_code %_% ")}"),
    author = "Investigator: " %_% investigator,
    date = NULL
  )

  make_sweave_document(file_path_base %_% ".snw", output_path=output_path, header=header, document=document, bib=FALSE)
}


### Some corrections to the "Hmisc" package's 'latex()' functions.

latex_correct_insert_bottom <- function(l)
{
  ## The following is to fix a problem with Hmisc 3.14-6 in which definitions at the bottom of tables created by 'latex.summary.reverse()' aren't separated by LaTeX newlines.
  l[length(l)] <- gsub("\\.", "\\.\\\\\\\\", l[length(l)])
  l[length(l)] <- gsub("\\:", "\\:\\\\\\\\", l[length(l)])

  l
}


latex_correct_caption_position <- function(l)
{
  ## Move caption outside "size" brackets so that the short caption can show up in PDF bookmarks.
  captionIndex <- grep("^\\\\caption", l)
  if (length(captionIndex) > 0L) {
    captionIndex <- head(captionIndex, 1L)
    relsizeIndex <- grep("^\\{\\\\(relsize|larger|smaller|relscale|textlarger|textsmaller|textscale)", l)
    if (length(relsizeIndex) > 0L) {
      relsizeIndex <- head(relsizeIndex, 1L)
      if (captionIndex == relsizeIndex + 1L) {
        temp <- l[captionIndex]
        l[captionIndex] <- l[relsizeIndex]
        l[relsizeIndex] <- temp
      }
    }
  }

  l
}


### R Markdown

#' @export
rmd_print <- function(x, ...)
  UseMethod("rmd_print")


#' @export
stub_fun <- function(...) invisible()


#' @export
rmd_print.summaryM <- function(x, header_text="\\\\n\\\\nDescriptive Statistics", use_default_printout=TRUE, ...)
{
  fun <- Hmisc::print.summaryM

  ## Alter the body of 'fun' for our purposes.
  bodyText <- format(body(fun))

  ## Remove leading spaces from the numerical columns of the summary.
  bodyText <- sub("(cstats <- paste\\(spaces)", "# \\1", bodyText, perl=TRUE)

  if (!missing(header_text))
    bodyText <- sub(formals()$header_text, header_text, bodyText, perl=TRUE)

  ## Add total population 'N' as attribute.
  bodyText <- append(bodyText, "attr(cstats, 'N') <- x$N", after=grep("dimnames\\(cstats\\) <-", bodyText, perl=TRUE)[1])

  body(fun) <- parse(text=paste(bodyText, collapse="\n"))

  if (use_default_printout)
    r <- fun(x, ...)
  else {
    dev_null <- capture.output(r <- fun(x, ...))
    ## This is a stub for further development; see 'print.summaryM()' for details.
    print.char.matrix(r, col.names=FALSE, col.txt.align="left")
    ## I could try altering the 'capture.output()' of this into R Markdown "multiline" tables;
    ## v. http://rapporter.github.io/pander/#markdown-tables
  }

  invisible(r)
}


#' @export
rmd_print.default <- function(x, ..., kind=c("print", "kable", "pander"))
{
  kind <- match.arg(kind)
  rmdFun <- switch(kind,
    print = print,
    kable = knitr::kable,
    pander = pander::pander
  )

  rmdFun(x, ...)
}
